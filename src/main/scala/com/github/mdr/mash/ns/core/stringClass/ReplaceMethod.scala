package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.SameStringMethodTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashBoolean, MashString, MashValue }

object ReplaceMethod extends MashMethod("replace") {

  object Params {
    val Target = Parameter(
      nameOpt = Some("target"),
      summaryOpt = Some("String to replace"))
    val Replacement = Parameter(
      nameOpt = Some("replacement"),
      summaryOpt = Some("Replacement string"))
    val Regex = Parameter(
      nameOpt = Some("regex"),
      shortFlagOpt = Some('r'),
      summaryOpt = Some("Interpret target as a regular expression"),
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isFlag = true,
      isBooleanFlag = true)

  }

  import Params._

  val params = ParameterModel(Seq(Target, Replacement, Regex))

  def apply(target: MashValue, boundParams: BoundParams): MashString = {
    val s = target.asInstanceOf[MashString]
    val regex = boundParams(Regex).isTruthy
    val targetString = boundParams.validateString(Target).s
    val replacement = boundParams.validateString(Replacement).s
    if (regex)
      s.modify(_.replaceAll(targetString, replacement))
    else
      s.modify(_.replace(targetString, replacement))
  }

  override def typeInferenceStrategy = SameStringMethodTypeInferenceStrategy

  override def summaryOpt = Some("Replace occurrences of a string with another")

}
