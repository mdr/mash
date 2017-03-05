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

class GeneralArgBinder[T](params: ParameterModel,
                          arguments: Seq[GeneralArgument[T]]) {

  private var parameterToArguments: Map[Parameter, Seq[T]] = Map()
  private var lastParameterConsumed = false
  private var posToParam: Map[Int, Parameter] = Map()

  case class ArgAndPos(arg: GeneralArgument[T], pos: Int)

  case class PositionArgAndPos(arg: GeneralArgument.PositionArg[T], pos: Int)

  @throws[ArgBindingException[T]]
  def bind: GeneralArgBinderResult[T] = {
    handleLastArg()
    handlePositionalArgs()
    handleFlagArgs()
    handleDefaultAndMandatory()
    handleNamedArgsParams()
    handleAllArgsParams()
    GeneralArgBinderResult(parameterToArguments)
  }

  private def addArgToParam(param: Parameter, argAndPos: PositionArgAndPos) {
    addArgToParam(param, argAndPos.arg, argAndPos.pos)
  }

  private def addArgToParam(param: Parameter, arg: GeneralArgument[T], pos: Int) {
    ensureParamIsBound(param)
    parameterToArguments += param -> (parameterToArguments(param) :+ arg.value)
    posToParam += pos -> param
  }

  private def ensureParamIsBound(param: Parameter) =
    parameterToArguments += param -> parameterToArguments.getOrElse(param, Seq())

  private def handleNamedArgsParams() =
    for (param ← params.params if param.isNamedArgsParam) {
      ensureParamIsBound(param)
      for (ArgAndPos(arg, pos) ← argsAndPos if !posToParam.contains(pos) if !arg.isPositionArg)
        addArgToParam(param, arg, pos)
    }

  private def handleAllArgsParams() =
    for (param ← params.params if param.isAllArgsParam) {
      ensureParamIsBound(param)
      for (ArgAndPos(arg, pos) ← argsAndPos if !posToParam.contains(pos))
        addArgToParam(param, arg, pos)
    }

  private def handleLastArg() =
    for {
      lastParam ← params.lastParamOpt
      if !lastParam.nameOpt.exists(isProvidedAsNamedArg)
      paramName = lastParam.nameOpt
      argAndPos ← positionArgsWithIndex.lastOption
    } {
      lastParameterConsumed = true
      addArgToParam(lastParam, argAndPos)
    }

  private def handlePositionalArgs() {
    val regularParams = params.positionalParams.filterNot(p ⇒ p.isVariadic || p.isLast)
    val positionArgs = if (lastParameterConsumed) this.positionArgsWithIndex.init else this.positionArgsWithIndex

    handleExcessArguments(positionArgs, regularParams)

    for ((param, argAndPos) ← regularParams zip positionArgs)
      addArgToParam(param, argAndPos)
  }

  private def handleExcessArguments(positionArgs: Seq[PositionArgAndPos], regularParams: Seq[Parameter]) =
    if (positionArgs.size > regularParams.size)
      params.variadicParamOpt match {
        case Some(variadicParam) ⇒
          val varargs = positionArgs.drop(regularParams.size)
          for (argAndPos ← varargs)
            addArgToParam(variadicParam, argAndPos)
        case None                ⇒
          if (!hasAllArgsParam) {
            val maxPositionArgs = params.positionalParams.size
            val providedArgs = this.positionArgsWithIndex.size
            val firstExcessArgument = this.positionArgsWithIndex.drop(maxPositionArgs).head
            val wasWere = if (providedArgs == 1) "was" else "were"
            val isAre = if (maxPositionArgs == 1) "is" else "are"
            val message = s"Too many positional arguments -- $providedArgs $wasWere provided, but at most $maxPositionArgs $isAre allowed"
            throw new ArgBindingException(message, Some(firstExcessArgument.arg.value))
          }
      }

  private def handleFlagArgs() =
    for (ArgAndPos(argument, pos) ← argsAndPos)
      argument match {
        case GeneralArgument.ShortFlag(flags, _)    ⇒
          for (flag ← flags)
            bindFlagParam(flag, argument, pos)
        case GeneralArgument.LongFlag(flag, _)      ⇒
          bindFlagParam(flag, argument, pos)
        case posArg: GeneralArgument.PositionArg[T] ⇒
        // skip
      }

  private def bindFlagParam(paramName: String, arg: GeneralArgument[T], pos: Int) = {
    params.paramByName.get(paramName) match {
      case Some(param) ⇒
        if (!param.isNamedArgsParam)
          if (parameterToArguments contains param)
            throw new ArgBindingException(s"${describe(param).capitalize} is provided multiple times", Some(arg))
          else
            addArgToParam(param, arg, pos)
      case None        ⇒
        if (!hasNamedArgsParam && !hasAllArgsParam)
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
          else if (!param.isNamedArgsParam && !param.isAllArgsParam)
            throw new ArgBindingException(s"Missing mandatory ${describe(param)}")
      }


  private def describe(param: Parameter) = param.nameOpt match {
    case Some(name) ⇒ s"argument '$name'"
    case None       ⇒ "anonymous argument"
  }

  private def hasNamedArgsParam = params.params.exists(_.isNamedArgsParam)
  private def hasAllArgsParam = params.params.exists(_.isAllArgsParam)

  private lazy val argsAndPos: Seq[ArgAndPos] = arguments.zipWithIndex.map((ArgAndPos.apply _).tupled)

  private lazy val positionArgsWithIndex =
    argsAndPos.collect { case ArgAndPos(arg: GeneralArgument.PositionArg[T], pos) => PositionArgAndPos(arg, pos) }

  private def isProvidedAsNamedArg(name: String): Boolean =
    arguments.exists {
      case GeneralArgument.LongFlag(`name`, _) ⇒ true
      case GeneralArgument.ShortFlag(flags, _) ⇒ flags contains name
      case _                                   ⇒ false
    }

}