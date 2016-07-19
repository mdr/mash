package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime._

class ParamValidationContext(params: ParameterModel, arguments: Arguments, ignoreAdditionalParameters: Boolean) {

  private var boundParams: Map[String, MashValue] = Map()
  private var argumentNodes: Map[String, Seq[Argument]] = Map()
  private var lastParameterConsumed = false

  def validate(): BoundParams = {
    handleLastArg()
    handlePositionalArgs()
    handleFlagArgs()
    handleDefaultAndMandatory()
    BoundParams(boundParams, argumentNodes)
  }

  private def addArgumentNode(paramName: String, argNode: Argument) {
    argumentNodes += paramName -> (argumentNodes.getOrElse(paramName, Seq()) :+ argNode)
  }

  private def handleLastArg() =
    for {
      lastParam ← params.lastParamOpt
      paramName = lastParam.name
      lastArg ← arguments.positionArgs.lastOption
      if !arguments.isProvidedAsNamedArg(paramName)
    } {
      lastParameterConsumed = true
      boundParams += paramName -> lastArg.value.resolve
      for (argNode ← lastArg.argumentNodeOpt)
        addArgumentNode(paramName, argNode)
    }

  private def handlePositionalArgs() {
    val regularPosParams = params.positionalParams.filterNot(p ⇒ p.isVariadic || p.isLast)
    val positionArgs = if (lastParameterConsumed) arguments.positionArgs.init else arguments.positionArgs

    handleExcessArguments(positionArgs, regularPosParams)

    for ((param, arg) ← regularPosParams zip positionArgs) {
      boundParams += param.name -> arg.value.resolve
      for (argNode ← arg.argumentNodeOpt)
        addArgumentNode(param.name, argNode)
    }
  }

  private def handleExcessArguments(positionArgs: Seq[EvaluatedArgument.PositionArg], regularPosParams: Seq[Parameter]) =
    if (positionArgs.size > regularPosParams.size)
      params.variadicParamOpt match {
        case Some(variadicParam) ⇒
          val varargs = positionArgs.drop(regularPosParams.size)
          boundParams += variadicParam.name -> MashList(varargs.map(_.value.resolve))
          for {
            firstVararg ← varargs
            argNode ← firstVararg.argumentNodeOpt
          } addArgumentNode(variadicParam.name, argNode)
        case None ⇒
          val maxPositionArgs = params.positionalParams.size
          val providedArgs = arguments.positionArgs.size
          if (!ignoreAdditionalParameters) {
            val firstExcessArgument = arguments.positionArgs.drop(maxPositionArgs).head
            val locationOpt = firstExcessArgument.argumentNodeOpt.flatMap(_.sourceInfoOpt).map(_.location)
            throw new ArgumentException(s"Too many arguments -- $providedArgs were provided, but at most $maxPositionArgs are allowed", locationOpt)
          }
      }

  private def handleFlagArgs() {
    for (argument ← arguments.evaluatedArguments)
      argument match {
        case EvaluatedArgument.ShortFlag(flags, argNodeOpt) ⇒
          for (flag ← flags)
            bindFlagParam(flag, argNodeOpt, value = MashBoolean.True)
        case EvaluatedArgument.LongFlag(flag, None, argNodeOpt) ⇒
          bindFlagParam(flag, argNodeOpt, value = MashBoolean.True)
        case EvaluatedArgument.LongFlag(flag, Some(value), argNodeOpt) ⇒
          bindFlagParam(flag, argNodeOpt, value.resolve)
        case posArg: EvaluatedArgument.PositionArg ⇒
        // handled elsewhere
      }
  }

  private def bindFlagParam(paramName: String, argNodeOpt: Option[Argument], value: MashValue) = {
    lazy val errorLocationOpt = argNodeOpt.flatMap(_.sourceInfoOpt).map(_.location)
    params.paramByName.get(paramName) match {
      case Some(param) ⇒
        if (boundParams contains param.name)
          throw new ArgumentException(s"Argument '${param.name}' is provided multiple times", errorLocationOpt)
        else {
          boundParams += param.name -> value
          for (argNode ← argNodeOpt)
            addArgumentNode(param.name, argNode)
        }
      case None ⇒
        if (!ignoreAdditionalParameters)
          throw new ArgumentException(s"Unexpected named argument '$paramName'", errorLocationOpt)
    }
  }

  private def handleDefaultAndMandatory() =
    for (param ← params.params if !boundParams.contains(param.name))
      param.defaultValueGeneratorOpt match {
        case Some(generator) ⇒
          boundParams += param.name -> generator()
        case None ⇒
          if (param.isVariadic)
            if (param.variadicAtLeastOne)
              throw new ArgumentException(s"Missing mandatory argument '${param.name}'")
            else
              boundParams += param.name -> MashList.empty
          else
            throw new ArgumentException(s"Missing mandatory argument '${param.name}'")
      }

}