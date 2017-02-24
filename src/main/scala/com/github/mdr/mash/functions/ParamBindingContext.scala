package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime._

/**
  * Bind arguments to parameters
  */
class ParamBindingContext(params: ParameterModel, arguments: Arguments, ignoreAdditionalParameters: Boolean) {

  private var boundNames: Map[String, MashValue] = Map()
  private var boundParams: Set[Parameter] = Set()
  private var parameterToArguments: Map[Parameter, Seq[Argument]] = Map()
  private var lastParameterConsumed = false

  def validate(): BoundParams = {
    handleNamedArgsParams()
    handleLastArg()
    handlePositionalArgs()
    handleFlagArgs()
    handleDefaultAndMandatory()
    BoundParams(boundNames, parameterToArguments)
  }

  private def addArgumentToParameter(parameter: Parameter, argNode: Argument) {
    parameterToArguments += parameter -> (parameterToArguments.getOrElse(parameter, Seq()) :+ argNode)
  }

  private def handleNamedArgsParams(): Unit = {
    lazy val namedArguments = MashObject.of(arguments.evaluatedArguments.flatMap {
      case EvaluatedArgument.LongFlag(flag, valueOpt, _) ⇒ Seq(flag -> valueOpt.map(_.resolve()).getOrElse(MashBoolean.True))
      case EvaluatedArgument.ShortFlag(flags, _)         ⇒ flags.map(c ⇒ c.toString -> MashBoolean.True)
      case EvaluatedArgument.PositionArg(_, _)           ⇒ Seq()
    })
    val argumentNodes = arguments.evaluatedArguments
      .filterNot(_.isInstanceOf[EvaluatedArgument.PositionArg])
      .flatMap(_.argumentNodeOpt)
    for (param ← params.params if param.isNamedArgsParam) {
      param.nameOpt.foreach(boundNames += _ -> namedArguments)
      boundParams += param
      parameterToArguments += (param -> argumentNodes)
    }
  }

  private def handleLastArg() =
    for {
      lastParam ← params.lastParamOpt
      paramName = lastParam.nameOpt
      lastArg ← arguments.positionArgs.lastOption
      if !lastParam.nameOpt.exists(arguments.isProvidedAsNamedArg)
    } {
      lastParameterConsumed = true
      bindParam(lastParam, resolve(lastParam, lastArg.value), lastArg)
      for (argNode ← lastArg.argumentNodeOpt)
        addArgumentToParameter(lastParam, argNode)
    }

  private def handlePositionalArgs() {
    val regularParams = params.positionalParams.filterNot(p ⇒ p.isVariadic || p.isLast)
    val positionArgs = if (lastParameterConsumed) arguments.positionArgs.init else arguments.positionArgs

    handleExcessArguments(positionArgs, regularParams)

    for ((param, arg) ← regularParams zip positionArgs) {
      bindParam(param, resolve(param, arg.value), arg)
      for (argNode ← arg.argumentNodeOpt)
        addArgumentToParameter(param, argNode)
    }
  }

  private def bindParam(param: Parameter, value: MashValue, arg: EvaluatedArgument) {
    param.patternOpt match {
      case Some(pattern) ⇒ bindPattern(pattern, value, getLocation(arg))
      case None          ⇒ param.nameOpt.foreach(boundNames += _ -> value)
    }
    boundParams += param
  }

  private def bindPattern(pattern: ParamPattern, value: MashValue, locationOpt: Option[SourceLocation] = None): Unit =
    pattern match {
      case ParamPattern.Object(entries)   ⇒
        value match {
          case obj: MashObject ⇒
            for (entry <- entries)
              entry match {
                case ParamPattern.ObjectEntry(fieldName, None)               ⇒
                  boundNames += fieldName -> obj.get(fieldName).getOrElse(MashNull)
                case ParamPattern.ObjectEntry(fieldName, Some(valuePattern)) ⇒
                  bindPattern(valuePattern, obj.get(fieldName).getOrElse(MashNull), locationOpt)
              }
          case _               ⇒
            throw new ArgumentException(s"Cannot match object pattern against value of type " + value.typeName, locationOpt)
        }
      case ParamPattern.Hole              ⇒
      case ParamPattern.Ident(identifier) ⇒
        boundNames += identifier -> value
      case ParamPattern.List(patterns)    ⇒
        value match {
          case list: MashList ⇒
            for ((elementOpt, elementPattern) ← list.elements.map(Some(_)).padTo(patterns.length, None).zip(patterns))
              bindPattern(elementPattern, elementOpt.getOrElse(MashNull), locationOpt)
          case _              ⇒
            throw new ArgumentException(s"Cannot match list pattern against value of type " + value.typeName, locationOpt)
        }

    }

  private def resolve(param: Parameter, suspendedValue: SuspendedMashValue): MashValue =
    if (param.isLazy)
      SuspendedValueFunction(suspendedValue)
    else
      suspendedValue.resolve()

  private def hasNamedArgsParam = params.params.exists(_.isNamedArgsParam)

  private def handleExcessArguments(positionArgs: Seq[EvaluatedArgument.PositionArg], regularParams: Seq[Parameter]) =
    if (positionArgs.size > regularParams.size)
      params.variadicParamOpt match {
        case Some(variadicParam) ⇒
          val varargs = positionArgs.drop(regularParams.size)
          variadicParam.nameOpt.foreach(boundNames += _ -> MashList(varargs.map(_.value.resolve())))
          boundParams += variadicParam
          for {
            firstVararg ← varargs
            argNode ← firstVararg.argumentNodeOpt
          } addArgumentToParameter(variadicParam, argNode)
        case None                ⇒
          val maxPositionArgs = params.positionalParams.size
          val providedArgs = arguments.positionArgs.size
          if (!ignoreAdditionalParameters) {
            val firstExcessArgument = arguments.positionArgs.drop(maxPositionArgs).head
            val locationOpt = getLocation(firstExcessArgument)
            val wasWere = if (providedArgs == 1) "was" else "were"
            val isAre = if (maxPositionArgs == 1) "is" else "are"
            val message = s"Too many positional arguments -- $providedArgs $wasWere provided, but at most $maxPositionArgs $isAre allowed"
            throw new ArgumentException(message, locationOpt)
          }
      }

  private def getLocation(arg: EvaluatedArgument): Option[SourceLocation] =
    arg.argumentNodeOpt.flatMap(_.sourceInfoOpt).map(_.location)

  private def handleFlagArgs() {
    for (argument ← arguments.evaluatedArguments)
      argument match {
        case EvaluatedArgument.ShortFlag(flags, argNodeOpt)            ⇒
          for (flag ← flags)
            bindFlagParam(flag, argNodeOpt, value = SuspendedMashValue(() ⇒ MashBoolean.True))
        case EvaluatedArgument.LongFlag(flag, None, argNodeOpt)        ⇒
          bindFlagParam(flag, argNodeOpt, value = SuspendedMashValue(() ⇒ MashBoolean.True))
        case EvaluatedArgument.LongFlag(flag, Some(value), argNodeOpt) ⇒
          bindFlagParam(flag, argNodeOpt, value)
        case posArg: EvaluatedArgument.PositionArg                     ⇒
        // handled elsewhere
      }
  }

  private def bindFlagParam(paramName: String, argNodeOpt: Option[Argument], value: SuspendedMashValue) = {
    lazy val errorLocationOpt = argNodeOpt.flatMap(_.sourceInfoOpt).map(_.location)
    params.paramByName.get(paramName) match {
      case Some(param) ⇒
        if (!param.isNamedArgsParam)
          if (boundParams contains param)
            throw new ArgumentException(s"${describe(param).capitalize} is provided multiple times", errorLocationOpt)
          else {
            param.nameOpt.foreach(boundNames += _ -> resolve(param, value))
            boundParams += param

            for (argNode ← argNodeOpt)
              addArgumentToParameter(param, argNode)
          }
      case None        ⇒
        if (!ignoreAdditionalParameters && !hasNamedArgsParam)
          throw new ArgumentException(s"Unexpected named argument '$paramName'", errorLocationOpt)
    }
  }

  private def handleDefaultAndMandatory() =
    for (param ← params.params if !boundParams.contains(param))
      param.defaultValueGeneratorOpt match {
        case Some(generator) ⇒
          val defaultValue = generator()
          param.patternOpt match {
            case Some(pattern) ⇒ bindPattern(pattern, defaultValue)
            case None          ⇒ param.nameOpt.foreach(boundNames += _ -> defaultValue)
          }
          boundParams += param
        case None            ⇒
          if (param.isVariadic)
            if (param.variadicAtLeastOne)
              throw new ArgumentException(s"Missing mandatory ${describe(param)}")
            else {
              param.nameOpt.foreach(boundNames += _ -> MashList.empty)
              boundParams += param
            }
          else
            throw new ArgumentException(s"Missing mandatory ${describe(param)}")
      }

  private def describe(param: Parameter) = param.nameOpt match {
    case Some(name) ⇒ s"argument '$name'"
    case None       ⇒ "anonymous argument"
  }

}

