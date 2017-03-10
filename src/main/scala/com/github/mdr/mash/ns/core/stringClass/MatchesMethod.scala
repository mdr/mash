package com.github.mdr.mash.ns.core.stringClass

import java.util.regex.Pattern

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.runtime.{ MashBoolean, MashString, MashValue }

object MatchesMethod extends MashMethod("matches") {

  object Params {
    val _Pattern = Parameter(
      nameOpt = Some("pattern"),
      summaryOpt = Some("Regular expression pattern"))
    val IgnoreCase = Parameter(
      nameOpt = Some("ignoreCase"),
      summaryOpt = Some("Perform a case-insensitive match"),
      shortFlagOpt = Some('i'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isBooleanFlag = true)
  }

  import Params._

  val params = ParameterModel(Seq(_Pattern, IgnoreCase))

  def apply(target: MashValue, boundParams: BoundParams): MashBoolean = {
    val s = target.asInstanceOf[MashString].s
    val pattern = ToStringifier.stringify(boundParams(_Pattern))
    val ignoreCase = boundParams(IgnoreCase).isTruthy
    val flags = if (ignoreCase) Pattern.CASE_INSENSITIVE else 0
    MashBoolean(Pattern.compile(pattern, flags).matcher(s).find)
  }

  override def typeInferenceStrategy = BooleanClass

  override def summaryOpt = Some("Test whether this string contains a match within it to a given regular expression")

}
