package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.SameStringMethodTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashBoolean, MashString, MashValue }
import com.github.mdr.mash.utils.Utils

object DeleteUpToMethod extends MashMethod("deleteUpTo") {

  object Params {
    val Substring = Parameter(
      nameOpt = Some("substring"),
      summaryOpt = Some("String to search for"))
    val AndIncluding = Parameter(
      nameOpt = Some("andIncluding"),
      summaryOpt = Some("If true, remove the substring as well (default false)"),
      shortFlagOpt = Some('i'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(false),
      isBooleanFlag = true)
  }

  import Params._

  val params = ParameterModel(Substring, AndIncluding)

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    val s = target.asInstanceOf[MashString]
    val substring = boundParams.validateString(Substring)
    val andIncluding = boundParams(AndIncluding).isTruthy
    Utils.indexOf(s.s, substring.s) match {
      case Some(i) ⇒ s.modify(_.drop(i + (if (andIncluding) substring.length else 0)))
      case None    ⇒ s
    }
  }

  override def typeInferenceStrategy = SameStringMethodTypeInferenceStrategy

  override def summaryOpt = Some("Delete characters up to the first instance of a given substring")

  override def descriptionOpt = Some(
    """If no match is found, the string is returned unchanged.
      |
      |Examples:
      |  'result: { "value": 10 }'.deleteUpTo '{'        # '{ "value": 10 }'
      |  'value: 10'.deleteUpTo --andIncluding 'value: ' # '10'""".stripMargin)
}
