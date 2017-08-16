package com.github.mdr.mash.functions

import java.nio.file.Path

import com.github.mdr.mash.classes.{ BoundMethod, MashClass }
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.BoundParams.Function1Or2
import com.github.mdr.mash.ns.core.CharacterClass
import com.github.mdr.mash.ns.core.NoArgFunction.NoArgValue
import com.github.mdr.mash.parser.AbstractSyntax.Argument
import com.github.mdr.mash.runtime._

import scala.util.control.Exception._

object BoundParams {

  type Function1Or2 = Either[MashValue ⇒ MashValue, (MashValue, MashValue) ⇒ MashValue]

}


sealed trait Function1Or2Or3

object Function1Or2Or3 {

  case class One(f: MashValue ⇒ MashValue) extends Function1Or2Or3

  case class Two(f: (MashValue, MashValue) ⇒ MashValue) extends Function1Or2Or3

  case class Three(f: (MashValue, MashValue, MashValue) ⇒ MashValue) extends Function1Or2Or3

}

case class BoundParams(boundNames: Map[String, MashValue],
                       parameterToArguments: Map[Parameter, Seq[Argument]],
                       allResolvedArgs: Seq[EvaluatedArgument[MashValue]],
                       safeNames: Set[String]) {

  def apply(param: String): MashValue = boundNames(param)

  def apply(param: Parameter): MashValue = boundNames(param.nameOpt getOrElse "")

  @throws[EvaluatorException]
  def throwInvalidArgument(param: Parameter, message: String): Nothing = {
    val fullMessage = s"Invalid argument '${name(param)}'. $message"
    throw new ArgumentException(fullMessage, locationOpt(param))
  }

  private def name(param: Parameter): String = param.nameOpt getOrElse Parameter.AnonymousParamName

  private def mergeLocation(location1: SourceLocation, location2: SourceLocation): SourceLocation =
    SourceLocation(location1.provenance, location1.pointedRegion merge location2.pointedRegion)

  private def locationOpt(param: Parameter): Option[SourceLocation] =
    parameterToArguments.get(param).flatMap(nodes ⇒ nodes.flatMap(_.locationOpt).reduceOption(mergeLocation))

  def validateSequence(param: Parameter, allowStrings: Boolean = true): Seq[MashValue] = this (param) match {
    case xs: MashList                     ⇒ xs.elements
    case MashString(s, _) if allowStrings ⇒ s.toSeq.map(c ⇒ MashString(c.toString, Some(CharacterClass)))
    case x                                ⇒ throwInvalidArgumentType("sequence", x, param)
  }

  def validateString(param: Parameter): MashString = this (param) match {
    case s: MashString ⇒ s
    case x             ⇒ throwInvalidArgumentType("String", x, param)
  }

  def validateObject(param: Parameter): MashObject = this (param) match {
    case obj: MashObject ⇒ obj
    case x               ⇒ throwInvalidArgumentType("Object", x, param)
  }

  def validateObjectOpt(param: Parameter): Option[MashObject] = this (param) match {
    case obj: MashObject ⇒ Some(obj)
    case NoArgValue      ⇒ None
    case x               ⇒ throwInvalidArgumentType("Object", x, param)
  }

  def validateStringOpt(param: Parameter): Option[MashString] = this (param) match {
    case s: MashString ⇒ Some(s)
    case NoArgValue    ⇒ None
    case x             ⇒ throwInvalidArgumentType("String", x, param)
  }

  def validateFunctionOpt(param: Parameter): Option[MashValue ⇒ MashValue] =
    validateFunctionOpt(param, this (param))

  private def validateFunctionOpt(param: Parameter, value: MashValue): Option[MashValue ⇒ MashValue] =
    value match {
      case f@(_: MashString | _: MashFunction | _: BoundMethod) ⇒ Some(FunctionHelpers.interpretAsFunction(f))
      case NoArgValue                                           ⇒ None
      case x                                                    ⇒ throwInvalidArgumentType("function", x, param)
    }

  def validateFunction(param: Parameter, value: MashValue): MashValue ⇒ MashValue = value match {
    case f@(_: MashString | _: MashFunction | _: BoundMethod) ⇒ FunctionHelpers.interpretAsFunction(f)
    case x                                                    ⇒ throwInvalidArgumentType("function", x, param)
  }

  def validateFunction(param: Parameter): MashValue ⇒ MashValue = validateFunction(param, this (param))

  def validateFunction2(param: Parameter): (MashValue, MashValue) ⇒ MashValue = this (param) match {
    case f@(_: MashString | _: MashFunction | _: BoundMethod) ⇒
      FunctionHelpers.interpretAsFunction2(f)
    case x                                                    ⇒
      throwInvalidArgumentType("function", x, param)
  }

  def validateFunction1Or2(param: Parameter): Function1Or2 = validateFunction1Or2(this (param), param)

  def validateFunction1Or2(argument: MashValue, param: Parameter): Function1Or2 =
    argument match {
      case f: MashFunction if f.params.allowsTwoPositionalArguments         ⇒ Right(FunctionHelpers.interpretAsFunction2(f))
      case bm: BoundMethod if bm.method.params.allowsTwoPositionalArguments ⇒ Right(FunctionHelpers.interpretAsFunction2(bm))
      case arg                                                              ⇒ Left(validateFunction(param, arg))
    }

  def validateFunction1Or2Or3(param: Parameter): Function1Or2Or3 = {
    val argument = this (param)
    def detectArity(params: ParameterModel) =
      if (params.allowsAtLeastThisManyPositionalArguments(3))
        Function1Or2Or3.Three(FunctionHelpers.interpretAsFunction3(argument))
      else if (params.allowsAtLeastThisManyPositionalArguments(2))
        Function1Or2Or3.Two(FunctionHelpers.interpretAsFunction2(argument))
      else
        Function1Or2Or3.One(FunctionHelpers.interpretAsFunction(argument))
    argument match {
      case f: MashFunction ⇒ detectArity(f.params)
      case bm: BoundMethod ⇒ detectArity(bm.params)
      case _               ⇒ throwInvalidArgumentType("function", argument, param)
    }
  }

  private def throwInvalidArgumentType(desiredType: String, value: MashValue, param: Parameter) = {
    val message = s"Invalid argument '${name(param)}'. Must be a $desiredType, but was a ${value.typeName}"
    throw new ArgumentException(message, locationOpt(param))
  }

  def validateClass(param: Parameter): MashClass =
    this (param) match {
      case klass: MashClass ⇒ klass
      case x                ⇒ throwInvalidArgumentType("class", x, param)
    }

  def validatePath(param: Parameter): Path = {
    val arg = this (param)
    FunctionHelpers.safeInterpretAsPath(arg) match {
      case Some(path) ⇒ path
      case None       ⇒ throwInvalidArgumentType("path", arg, param)
    }
  }

  def validatePaths(param: Parameter): Seq[Path] = {
    val arg = this (param)
    catching(classOf[EvaluatorException]) opt FunctionHelpers.interpretAsPaths(arg) getOrElse (
      throw new ArgumentException(s"Invalid argument '${name(param)}', could not interpret value of type ${arg.typeName} as a path.", locationOpt(param)))
  }

  def validateInteger(param: Parameter): Int = validateInteger(param, this (param))

  def validateIntegerOpt(param: Parameter): Option[Int] =
    this (param) match {
      case NoArgValue ⇒ None
      case value      ⇒ Some(validateInteger(param, value))
    }

  def validateNonNegativeInteger(param: Parameter): Int = {
    val n = validateInteger(param)
    checkNonNegative(n, param)
    n
  }

  private def checkNonNegative(n: Int, param: Parameter): Unit = {
    if (n < 0) {
      val message = s"Invalid argument '${name(param)}'. Must be a positive integer, but was $n"
      throw new ArgumentException(message, locationOpt(param))
    }
  }

  def validateNonNegativeIntegerOpt(param: Parameter): Option[Int] = {
    val countOpt = validateIntegerOpt(param)
    countOpt.foreach(checkNonNegative(_, param))
    countOpt
  }

  private def validateInteger(param: Parameter, v: MashValue): Int = v match {
    case MashInteger(n) ⇒
      n
    case n: MashNumber  ⇒
      val message = s"Invalid argument '${name(param)}'. Must be an integer, but was '$n'"
      throw new ArgumentException(message, locationOpt(param))
    case x              ⇒
      val message = s"Invalid argument '${name(param)}'. Must be an integer, but was a ${x.typeName}"
      throw new ArgumentException(message, locationOpt(param))
  }

  def validateNumber(param: Parameter): Double = this (param) match {
    case MashNumber(n, _) ⇒
      n
    case value            ⇒
      val message = s"Invalid argument '${name(param)}'. Must be an integer, but was a ${value.typeName}"
      throw new ArgumentException(message, locationOpt(param))
  }

}