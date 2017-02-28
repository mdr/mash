package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.ParamPattern.{ List, Object }
import com.github.mdr.mash.runtime._

/**
  * Bind arguments to parameters
  */
class ParamBindingContext(params: ParameterModel, arguments: Arguments, ignoreAdditionalParameters: Boolean) {

  private var boundNames: Map[String, MashValue] = Map()

  def bind: BoundParams = {
    val parameterToArgs: Map[Parameter, Seq[EvaluatedArgument]] = runGeneralArgBinder

    bindParams(parameterToArgs)

    val parameterToArguments =
      for ((param, evalArgs) <- parameterToArgs)
        yield param -> evalArgs.flatMap(_.argumentNodeOpt)

    BoundParams(boundNames, parameterToArguments)
  }

  private def runGeneralArgBinder: Map[Parameter, Seq[EvaluatedArgument]] = {
    val argBinder = new GeneralArgBinder(params, generalArguments, ignoreAdditionalParameters)
    val result =
      try
        argBinder.bind
      catch {
        case ArgBindingException(message, argumentOpt) ⇒
          val locationOpt = argumentOpt.collect { case arg: EvaluatedArgument ⇒ arg }.flatMap(getLocation)
          throw new ArgumentException(message, locationOpt)
      }
    result.parameterToArguments
  }

  private def bindParams(parameterToArgs: Map[Parameter, Seq[EvaluatedArgument]]) =
    for ((param, evalArgs) ← parameterToArgs)
      if (param.isVariadic)
        bindVariadicParam(param, evalArgs)
      else if (param.isNamedArgsParam)
        bindNamedArgsParam(param, evalArgs)
      else
        bindRegularParam(param, evalArgs)

  private def generalArguments: Seq[GeneralArgument[EvaluatedArgument]] =
    arguments.evaluatedArguments.map {
      case arg@EvaluatedArgument.ShortFlag(flags, _)  ⇒ GeneralArgument.ShortFlag(flags, arg)
      case arg@EvaluatedArgument.LongFlag(flag, _, _) ⇒ GeneralArgument.LongFlag(flag, arg)
      case arg@EvaluatedArgument.PositionArg(_, _)    ⇒ GeneralArgument.PositionArg(arg)
    }

  private def getArgValue(param: Parameter, arg: EvaluatedArgument): MashValue = arg match {
    case EvaluatedArgument.PositionArg(value, _)    ⇒
      resolve(param, value)
    case EvaluatedArgument.LongFlag(_, valueOpt, _) ⇒
      valueOpt.map(resolve(param, _)) getOrElse MashBoolean.True
    case EvaluatedArgument.ShortFlag(_, _)          ⇒
      MashBoolean.True
  }

  private def bindVariadicParam(param: Parameter, evalArgs: Seq[EvaluatedArgument]): Unit = {
    val argsList =
      param.defaultValueGeneratorOpt.filter(_ ⇒ evalArgs.isEmpty) match {
        case Some(generator) ⇒
          generator()
        case None            ⇒
          evalArgs match {
            case Seq(arg@EvaluatedArgument.LongFlag(_, Some(_), _)) ⇒ getArgValue(param, arg) match {
              case xs: MashList ⇒ xs
              case x            ⇒
                throw new ArgumentException(s"A variadic parameter requires a List argument, but was given a " + x.typeName, getLocation(arg))
            }
            case _ ⇒ MashList(evalArgs.map(getArgValue(param, _)))
          }
      }
    for (name ← param.nameOpt)
      boundNames += name -> argsList
  }

  private def bindRegularParam(param: Parameter, evalArgs: Seq[EvaluatedArgument]) {
    val value = evalArgs match {
      case Seq()    ⇒
        param.defaultValueGeneratorOpt.getOrElse(throw new AssertionError(s"No argument for mandatory param $param"))()
      case Seq(arg) ⇒
        getArgValue(param, arg)
      case _        ⇒
        throw new AssertionError(s"Multiple arguments for param $param")
    }
    param.patternOpt match {
      case Some(pattern) ⇒ bindPattern(pattern, value)
      case None          ⇒ param.nameOpt.foreach(boundNames += _ -> value)
    }
  }

  private def bindNamedArgsParam(param: Parameter, evalArgs: Seq[EvaluatedArgument]) {
    val argsObject = MashObject.of(evalArgs.flatMap {
      case EvaluatedArgument.LongFlag(flag, valueOpt, _) ⇒
        val value = valueOpt.map(_.resolve()) getOrElse MashBoolean.True
        Seq(flag -> value)
      case EvaluatedArgument.ShortFlag(flags, _)         ⇒
        flags.map(_.toString -> MashBoolean.True)
      case EvaluatedArgument.PositionArg(_, _)           ⇒
        Seq()
    })
    for (name ← param.nameOpt)
      boundNames += name -> argsObject
  }

  private def bindPattern(pattern: ParamPattern, value: MashValue, locationOpt: Option[SourceLocation] = None): Unit =
    pattern match {
      case ParamPattern.Ident(identifier)     ⇒ boundNames += identifier -> value
      case ParamPattern.Hole                  ⇒
      case objectPattern: ParamPattern.Object ⇒ bindObjectPattern(objectPattern, locationOpt, value)
      case listPattern: ParamPattern.List     ⇒ bindListPatterns(listPattern, locationOpt, value)
    }

  private def bindListPatterns(listPattern: List, locationOpt: Option[SourceLocation], value: MashValue) =
    value match {
      case list: MashList ⇒
        val patterns = listPattern.patterns
        for ((elementOpt, elementPattern) ← list.elements.map(Some(_)).padTo(patterns.length, None).zip(patterns))
          bindPattern(elementPattern, elementOpt.getOrElse(MashNull), locationOpt)
      case _              ⇒
        throw new ArgumentException(s"Cannot match list pattern against value of type " + value.typeName, locationOpt)
    }

  private def bindObjectPattern(objectPattern: Object, locationOpt: Option[SourceLocation], value: MashValue) =
    value match {
      case obj: MashObject ⇒
        for (entry <- objectPattern.entries)
          entry match {
            case ParamPattern.ObjectEntry(fieldName, None)               ⇒
              boundNames += fieldName -> obj.get(fieldName).getOrElse(MashNull)
            case ParamPattern.ObjectEntry(fieldName, Some(valuePattern)) ⇒
              bindPattern(valuePattern, obj.get(fieldName).getOrElse(MashNull), locationOpt)
          }
      case _               ⇒
        throw new ArgumentException(s"Cannot match object pattern against value of type " + value.typeName, locationOpt)
    }

  private def resolve(param: Parameter, suspendedValue: SuspendedMashValue): MashValue =
    if (param.isLazy)
      SuspendedValueFunction(suspendedValue)
    else
      suspendedValue.resolve()

  private def getLocation(arg: EvaluatedArgument): Option[SourceLocation] =
    arg.argumentNodeOpt.flatMap(_.sourceInfoOpt).map(_.location)

}

