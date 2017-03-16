package com.github.mdr.mash.ns.core.stringClass

import java.util.regex.Pattern

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime._

object SplitMethod extends MashMethod("split") {

  object Params {
    val Regex = Parameter(
      nameOpt = Some("regex"),
      shortFlagOpt = Some('r'),
      summaryOpt = Some("Interpret separator as a regular expression; otherwise, interpret separator as the literal string (default false)"),
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isFlag = true,
      isBooleanFlag = true)
    val Separator = Parameter(
      nameOpt = Some("separator"),
      summaryOpt = Some("Separator to split string on; if not provided, the default is to split on whitespace"),
      defaultValueGeneratorOpt = Some(MashNull))
  }

  import Params._

  val params = ParameterModel(Seq(Regex, Separator))

  def apply(target: MashValue, boundParams: BoundParams): MashList = {
    val targetString = target.asInstanceOf[MashString]
    val regex = boundParams(Regex).isTruthy
    val separator = getSeparator(boundParams, Separator, regex)
    split(targetString, separator)
  }

  def split(string: MashString, separator: String): MashList = {
    val pieces = string.s.split(separator, -1)
    MashList(pieces.map(MashString(_, string.tagClassOpt)))
  }

  override def typeInferenceStrategy = (inferencer, targetTypeOpt, arguments) =>
    targetTypeOpt orElse Some(Type.Instance(StringClass)) map (_.seq)

  override def summaryOpt = Some("Split this string into a sequence of substrings using a separator")

  def getSeparator(boundParams: BoundParams, param: Parameter, regex: Boolean): String =
    boundParams(Separator) match {
      case MashNull                 ⇒ "\\s+"
      case MashString(separator, _) ⇒ if (regex) separator else Pattern.quote(separator)
      case x                        ⇒ boundParams.throwInvalidArgument(Separator, "Invalid separator of type " + x.typeName)
    }
}
