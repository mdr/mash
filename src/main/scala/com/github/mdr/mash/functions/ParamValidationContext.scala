package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime._

class ParamValidationContext(params: ParameterModel, arguments: Arguments, ignoreAdditionalParameters: Boolean) {

  private var boundNames: Map[String, MashValue] = Map()
  private var boundParams: Set[Parameter] = Set()
  private var argumentNodes: Map[Parameter, Seq[Argument]] = Map()
  private var lastParameterConsumed = false

  def validate(): BoundParams = {
    handleLastArg()
    handlePositionalArgs()
    handleFlagArgs()
    handleDefaultAndMandatory()
    BoundParams(boundNames, argumentNodes)
  }

  private def addArgumentNode(parameter: Parameter, argNode: Argument) {
    argumentNodes += parameter -> (argumentNodes.getOrElse(parameter, Seq()) :+ argNode)
  }

  private def handleLastArg() =
    for {
      lastParam ← params.lastParamOpt
      paramName = lastParam.name
      lastArg ← arguments.positionArgs.lastOption
      if !arguments.isProvidedAsNamedArg(paramName)
    } {
      lastParameterConsumed = true
      boundNames += paramName -> resolve(lastParam, lastArg.value)
      boundParams += lastParam
      for (argNode ← lastArg.argumentNodeOpt)
        addArgumentNode(lastParam, argNode)
    }

  private def handlePositionalArgs() {
    val regularParams = params.positionalParams.filterNot(p ⇒ p.isVariadic || p.isLast)
    val positionArgs = if (lastParameterConsumed) arguments.positionArgs.init else arguments.positionArgs

    handleExcessArguments(positionArgs, regularParams)

    for ((param, arg) ← regularParams zip positionArgs) {
      val value = resolve(param, arg.value)
      param.patternObjectNamesOpt match {
        case Some(patternObjectNames) =>
          value match {
            case obj: MashObject =>
              for (fieldName <- patternObjectNames)
                boundNames += fieldName -> obj.get(fieldName).getOrElse(MashNull)
            case _ =>
              throw new ArgumentException(s"Cannot match object pattern against value of type " + value.typeName, getLocation(arg))
          }
        case None =>
          boundNames += param.name -> resolve(param, arg.value)
          for (argNode ← arg.argumentNodeOpt)
            addArgumentNode(param, argNode)
      }
      boundParams += param
    }
  }

  private def resolve(param: Parameter, suspendedValue: SuspendedMashValue): MashValue =
    if (param.isLazy)
      SuspendedValueFunction(suspendedValue)
    else
      suspendedValue.resolve()

  private def handleExcessArguments(positionArgs: Seq[EvaluatedArgument.PositionArg], regularParams: Seq[Parameter]) =
    if (positionArgs.size > regularParams.size)
      params.variadicParamOpt match {
        case Some(variadicParam) ⇒
          val varargs = positionArgs.drop(regularParams.size)
          boundNames += variadicParam.name -> MashList(varargs.map(_.value.resolve()))
          boundParams += variadicParam
          for {
            firstVararg ← varargs
            argNode ← firstVararg.argumentNodeOpt
          } addArgumentNode(variadicParam, argNode)
        case None ⇒
          val maxPositionArgs = params.positionalParams.size
          val providedArgs = arguments.positionArgs.size
          if (!ignoreAdditionalParameters) {
            val firstExcessArgument = arguments.positionArgs.drop(maxPositionArgs).head
            val locationOpt = getLocation(firstExcessArgument)
            throw new ArgumentException(s"Too many arguments -- $providedArgs were provided, but at most $maxPositionArgs are allowed", locationOpt)
          }
      }

  private def getLocation(arg: EvaluatedArgument) = arg.argumentNodeOpt.flatMap(_.sourceInfoOpt).map(_.location)

  private def handleFlagArgs() {
    for (argument ← arguments.evaluatedArguments)
      argument match {
        case EvaluatedArgument.ShortFlag(flags, argNodeOpt) ⇒
          for (flag ← flags)
            bindFlagParam(flag, argNodeOpt, value = SuspendedMashValue(() => MashBoolean.True))
        case EvaluatedArgument.LongFlag(flag, None, argNodeOpt) ⇒
          bindFlagParam(flag, argNodeOpt, value = SuspendedMashValue(() => MashBoolean.True))
        case EvaluatedArgument.LongFlag(flag, Some(value), argNodeOpt) ⇒
          bindFlagParam(flag, argNodeOpt, value)
        case posArg: EvaluatedArgument.PositionArg ⇒
        // handled elsewhere
      }
  }

  private def bindFlagParam(paramName: String, argNodeOpt: Option[Argument], value: SuspendedMashValue) = {
    lazy val errorLocationOpt = argNodeOpt.flatMap(_.sourceInfoOpt).map(_.location)
    params.paramByName.get(paramName) match {
      case Some(param) ⇒
        if (boundParams contains param)
          throw new ArgumentException(s"Argument '${param.name}' is provided multiple times", errorLocationOpt)
        else {
          boundNames += param.name -> resolve(param, value)
          boundParams += param

          for (argNode ← argNodeOpt)
            addArgumentNode(param, argNode)
        }
      case None ⇒
        if (!ignoreAdditionalParameters)
          throw new ArgumentException(s"Unexpected named argument '$paramName'", errorLocationOpt)
    }
  }

  private def handleDefaultAndMandatory() =
    for (param ← params.params if !boundParams.contains(param))
      param.defaultValueGeneratorOpt match {
        case Some(generator) ⇒
          boundNames += param.name -> generator()
          boundParams += param
        case None ⇒
          if (param.isVariadic)
            if (param.variadicAtLeastOne)
              throw new ArgumentException(s"Missing mandatory argument '${param.name}'")
            else {
              boundNames += param.name -> MashList.empty
              boundParams += param
            }
          else
            throw new ArgumentException(s"Missing mandatory argument '${param.name}'")
      }

}

