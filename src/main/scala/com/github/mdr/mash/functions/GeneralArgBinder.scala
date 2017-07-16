package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.Suggestor

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

case class GeneralArgBinderResult[T](parameterToArguments: Map[Parameter, Seq[T]] = Map(),
                                     posToParam: Map[Int, Parameter])

case class ArgBindingException[T](message: String, argumentOpt: Option[T] = None) extends RuntimeException(message)

class GeneralArgBinder[T](params: ParameterModel,
                          arguments: Seq[GeneralArgument[T]],
                          forgiving: Boolean = false) {

  private var parameterToArguments: Map[Parameter, Seq[T]] = Map()
  private var posToParam: Map[Int, Parameter] = Map()

  case class ArgAndPos(arg: GeneralArgument[T], pos: Int)

  case class PositionArgAndPos(arg: GeneralArgument.PositionArg[T], pos: Int)

  case class PositionalParameterSet(initialMandatory: Seq[Parameter],
                                    initialOptional: Seq[Parameter],
                                    variadicOrAllArgsOpt: Option[Parameter],
                                    finalOptional: Seq[Parameter],
                                    finalMandatory: Seq[Parameter]) {
    def maxNumberOfPositionalArgs = initialMandatory.size + initialOptional.size + finalOptional.size + finalMandatory.size
  }

  private lazy val positionalParameterSet: PositionalParameterSet = {
    def isRegular(p: Parameter) = !p.isVariadic && !p.isAllArgsParam
    def isOptional(p: Parameter) = isRegular(p) && p.hasDefault
    def isMandatory(p: Parameter) = isRegular(p) && !p.hasDefault
    val (initialMandatory, rest) = params.positionalParams.filterNot(isProvidedAsNamedArg).span(isMandatory)
    val (finalMandatory, rest2) = rest.reverse.span(isMandatory)
    val (initialOptional, rest3) = rest2.reverse.span(isOptional)
    val (finalOptional, rest4) = rest3.reverse.span(isOptional)
    val variadicOpt = rest4 match {
      case Seq(p) if p.isVariadic || p.isAllArgsParam ⇒ Some(p)
      case Seq()                                      ⇒ None
      case _ if forgiving                             ⇒ None
      case _                                          ⇒ throw new ArgBindingException[T](s"Bad parameters")
    }
    PositionalParameterSet(initialMandatory, initialOptional, variadicOpt, finalOptional, finalMandatory)
  }

  private def handlePositional() {
    val args1 = positionArgAndPos

    for ((param, argAndPos) ← positionalParameterSet.initialMandatory zip args1)
      addArgToParam(param, argAndPos)
    val args2 = args1.drop(positionalParameterSet.initialMandatory.length)

    for ((param, argAndPos) ← positionalParameterSet.finalMandatory.reverse zip args2.reverse)
      addArgToParam(param, argAndPos)
    val args3 = args2.dropRight(positionalParameterSet.finalMandatory.length)

    for ((param, argAndPos) ← positionalParameterSet.initialOptional zip args3)
      addArgToParam(param, argAndPos)
    val args4 = args3.drop(positionalParameterSet.initialOptional.length)

    for ((param, argAndPos) ← positionalParameterSet.finalOptional.reverse zip args4.reverse)
      addArgToParam(param, argAndPos)
    val args5 = args4.dropRight(positionalParameterSet.finalOptional.length)

    if (args5.nonEmpty)
      positionalParameterSet.variadicOrAllArgsOpt match {
        case Some(variadicOrAllArgsParam) ⇒
          if (variadicOrAllArgsParam.isVariadic)
            for (argAndPos ← args5)
              addArgToParam(variadicOrAllArgsParam, argAndPos)
          // else: allArgs param is handled later
        case None ⇒
          handleTooManyPositionalArguments()
      }
  }

  private def handleTooManyPositionalArguments() {
    val maxPositionArgs = positionalParameterSet.maxNumberOfPositionalArgs
    val providedArgs = positionArgAndPos.size
    val firstExcessArgument = positionArgAndPos.drop(maxPositionArgs).head // should be at least one if this is called
    val wasWere = if (providedArgs == 1) "was" else "were"
    val isAre = if (maxPositionArgs == 1) "is" else "are"
    val message = s"Too many positional arguments -- $providedArgs $wasWere provided, but at most $maxPositionArgs $isAre allowed"
    if (!forgiving)
      throw new ArgBindingException(message, Some(firstExcessArgument.arg.value))
  }

  @throws[ArgBindingException[T]]
  def bind: GeneralArgBinderResult[T] = {
    handlePositional()
    handleFlagArgs()
    handleDefaultAndMandatory()
    handleNamedArgsParams()
    handleAllArgsParams()
    GeneralArgBinderResult(parameterToArguments, posToParam)
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
      for (ArgAndPos(arg, pos) ← argsAndPos if !posToParam.contains(pos) && !arg.isPositionArg)
        addArgToParam(param, arg, pos)
    }

  private def handleAllArgsParams() =
    for (param ← params.params if param.isAllArgsParam) {
      ensureParamIsBound(param)
      for (ArgAndPos(arg, pos) ← argsAndPos if !posToParam.contains(pos))
        addArgToParam(param, arg, pos)
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

  private def bindFlagParam(paramName: String, arg: GeneralArgument[T], pos: Int): Unit =
    params.paramByName.get(paramName) match {
      case Some(param) ⇒
        if (!param.isNamedArgsParam)
          if (parameterToArguments contains param) {
            if (!forgiving)
              throw new ArgBindingException(s"${describe(param).capitalize} is provided multiple times", Some(arg))
          } else
            addArgToParam(param, arg, pos)
      case None        ⇒
        if (!hasNamedArgsParam && !hasAllArgsParam)
          if (!forgiving) {
            val names = params.paramByName.keys.toSeq
            val message = s"Unexpected named argument '$paramName'${Suggestor.suggestionSuffix(names, paramName)}"
            throw new ArgBindingException(message, Some(arg.value))
          }
    }

  private def handleDefaultAndMandatory() =
    for (param ← params.params if !(parameterToArguments contains param))
      param.defaultValueGeneratorOpt match {
        case Some(generator) ⇒
          ensureParamIsBound(param)
        case None            ⇒
          if (param.isVariadic)
            if (param.variadicAtLeastOne) {
              if (!forgiving)
                throw new ArgBindingException(s"Missing mandatory ${describe(param)}")
            } else
              ensureParamIsBound(param)
          else if (!param.isNamedArgsParam && !param.isAllArgsParam)
            if (!forgiving)
              throw new ArgBindingException(s"Missing mandatory ${describe(param)}")
      }


  private def describe(param: Parameter) = param.nameOpt match {
    case Some(name) ⇒ s"argument '$name'"
    case None       ⇒ "anonymous argument"
  }

  private def hasNamedArgsParam = params.params.exists(_.isNamedArgsParam)

  private def hasAllArgsParam = params.params.exists(_.isAllArgsParam)

  private lazy val argsAndPos: Seq[ArgAndPos] = arguments.zipWithIndex.map((ArgAndPos.apply _).tupled)

  private lazy val positionArgAndPos: Seq[PositionArgAndPos] =
    argsAndPos.collect { case ArgAndPos(arg: GeneralArgument.PositionArg[T], pos) => PositionArgAndPos(arg, pos) }

  private def isProvidedAsNamedArg(param: Parameter): Boolean =
    arguments.exists {
      case GeneralArgument.LongFlag(name, _)   ⇒ param.nameOpt contains name
      case GeneralArgument.ShortFlag(flags, _) ⇒ param.shortFlagOpt.map(_.toString) exists flags.contains
      case _                                   ⇒ false
    }
}