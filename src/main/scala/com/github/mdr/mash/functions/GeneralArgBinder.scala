package com.github.mdr.mash.functions

sealed trait GeneralArgument[+T] {

  val isPositionArg: Boolean

  val value: T

}

object GeneralArgument {

  case class PositionArg[T](value: T) extends GeneralArgument[T] {
    val isPositionArg = true
  }

  case class LongFlag[T](flag: String, value: T) extends GeneralArgument[T] {
    val isPositionArg = false
  }

  case class ShortFlag[T](flags: Seq[String], value: T) extends GeneralArgument[T] {
    val isPositionArg = false
  }

}

case class GeneralArgBinderResult[T](parameterToArguments: Map[Parameter, Seq[T]] = Map())

case class ArgBindingException[T](message: String, argumentOpt: Option[T] = None) extends RuntimeException(message)

class GeneralArgBinder[T](params: ParameterModel, arguments: Seq[GeneralArgument[T]], ignoreAdditionalParameters: Boolean) {

  private var parameterToArguments: Map[Parameter, Seq[T]] = Map()
  private var lastParameterConsumed = false

  @throws[ArgBindingException[T]]
  def bind: GeneralArgBinderResult[T] = {
    handleNamedArgsParams()
    handleLastArg()
    handlePositionalArgs()
    handleFlagArgs()
    handleDefaultAndMandatory()
    GeneralArgBinderResult(parameterToArguments)
  }

  private def addArgToParam(param: Parameter, arg: GeneralArgument[T]) = {
    ensureParamIsBound(param)
    parameterToArguments += param -> (parameterToArguments(param) :+ arg.value)
  }

  private def ensureParamIsBound(param: Parameter) =
    parameterToArguments += param -> parameterToArguments.getOrElse(param, Seq())

  private def handleNamedArgsParams() =
    for (param ← params.params if param.isNamedArgsParam) {
      ensureParamIsBound(param)
      arguments.filterNot(_.isPositionArg).foreach(addArgToParam(param, _))
    }

  private def handleLastArg() =
    for {
      lastParam ← params.lastParamOpt
      if !lastParam.nameOpt.exists(isProvidedAsNamedArg)
      paramName = lastParam.nameOpt
      lastArg ← positionArgs.lastOption
    } {
      lastParameterConsumed = true
      addArgToParam(lastParam, lastArg)
    }

  private def handlePositionalArgs() {
    val regularParams = params.positionalParams.filterNot(p ⇒ p.isVariadic || p.isLast)
    val positionArgs = if (lastParameterConsumed) this.positionArgs.init else this.positionArgs

    handleExcessArguments(positionArgs, regularParams)

    for ((param, arg) ← regularParams zip positionArgs)
      addArgToParam(param, arg)
  }

  private def handleExcessArguments(positionArgs: Seq[GeneralArgument.PositionArg[T]], regularParams: Seq[Parameter]) =
    if (positionArgs.size > regularParams.size)
      params.variadicParamOpt match {
        case Some(variadicParam) ⇒
          val varargs = positionArgs.drop(regularParams.size)
          for (arg ← varargs)
            addArgToParam(variadicParam, arg)
        case None                ⇒
          if (!ignoreAdditionalParameters) {
            val maxPositionArgs = params.positionalParams.size
            val providedArgs = this.positionArgs.size
            val firstExcessArgument = this.positionArgs.drop(maxPositionArgs).head
            val wasWere = if (providedArgs == 1) "was" else "were"
            val isAre = if (maxPositionArgs == 1) "is" else "are"
            val message = s"Too many positional arguments -- $providedArgs $wasWere provided, but at most $maxPositionArgs $isAre allowed"
            throw new ArgBindingException(message, Some(firstExcessArgument.value))
          }
      }

  private def handleFlagArgs() {
    for (argument ← arguments)
      argument match {
        case GeneralArgument.ShortFlag(flags, _)    ⇒
          for (flag ← flags)
            bindFlagParam(flag, argument)
        case GeneralArgument.LongFlag(flag, _)      ⇒
          bindFlagParam(flag, argument)
        case posArg: GeneralArgument.PositionArg[T] ⇒
        // skip
      }
  }


  private def bindFlagParam(paramName: String, arg: GeneralArgument[T]) = {
    params.paramByName.get(paramName) match {
      case Some(param) ⇒
        if (!param.isNamedArgsParam)
          if (parameterToArguments contains param)
            throw new ArgBindingException(s"${describe(param).capitalize} is provided multiple times", Some(arg))
          else
            addArgToParam(param, arg)
      case None        ⇒
        if (!ignoreAdditionalParameters && !hasNamedArgsParam)
          throw new ArgBindingException(s"Unexpected named argument '$paramName'", Some(arg.value))
    }
  }

  private def handleDefaultAndMandatory() =
    for (param ← params.params if !(parameterToArguments contains param))
      param.defaultValueGeneratorOpt match {
        case Some(generator) ⇒
          ensureParamIsBound(param)
        case None            ⇒
          if (param.isVariadic)
            if (param.variadicAtLeastOne)
              throw new ArgBindingException(s"Missing mandatory ${describe(param)}")
            else
              ensureParamIsBound(param)
          else
            throw new ArgBindingException(s"Missing mandatory ${describe(param)}")
      }


  private def describe(param: Parameter) = param.nameOpt match {
    case Some(name) ⇒ s"argument '$name'"
    case None       ⇒ "anonymous argument"
  }

  private def hasNamedArgsParam = params.params.exists(_.isNamedArgsParam)

  private lazy val positionArgs = arguments.collect { case arg: GeneralArgument.PositionArg[T] ⇒ arg }

  private def isProvidedAsNamedArg(name: String): Boolean =
    arguments.exists {
      case GeneralArgument.LongFlag(`name`, _) ⇒ true
      case GeneralArgument.ShortFlag(flags, _) ⇒ flags contains name
      case _                                   ⇒ false
    }

}