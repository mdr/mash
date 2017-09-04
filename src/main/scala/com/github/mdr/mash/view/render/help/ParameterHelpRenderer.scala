package com.github.mdr.mash.view.render.help

import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.utils.Utils._

object ParameterHelpRenderer extends AbstractHelpRenderer {

  def renderSection(parameters: Seq[Parameter]): Seq[Line] =
    if (parameters.nonEmpty) {
      val headingLines = Seq(Line.Empty, Line(SectionTitleStyle("PARAMETERS")))
      val paramLines = parameters.zipWithIndex.flatMap { case (param, i) ⇒
        val paddingLines = if (i > 0) Seq(Line.Empty) else Seq()
        paddingLines ++ renderParameterHelp(param)
      }
      headingLines ++ paramLines
    } else
      Seq()

  private def renderParameterHelp(param: Parameter): Seq[Line] = {
    val qualifierString = getParamQualifiers(param) match {
      case Seq()      ⇒ ""
      case qualifiers ⇒ qualifiers.mkString(" [", ", ", "]")
    }
    val name = param.nameOpt getOrElse Parameter.AnonymousParamName
    val paramName = ParamNameStyle(if (param.isFlag) "--" + name else name)
    val shortFlagDescription = param.shortFlagOpt.map(f ⇒ s" | -$f").getOrElse("").style(ParamNameStyle)
    val summaryDescription = param.summaryOpt.fold("")(" - " + _).style
    val summaryLine = Line(IndentSpace + paramName + shortFlagDescription + qualifierString.style + summaryDescription)
    val descriptionLines = param.descriptionOpt.toSeq.flatMap(description ⇒
      renderDescription(description, indentLevel = 2))
    summaryLine +: descriptionLines
  }

  private def getParamQualifiers(param: Parameter): Seq[String] =
    Seq(
      param.isLazy.option("lazy"),
      param.isNamedArgsParam.option("namedArgs"),
      param.isAllArgsParam.option("allArgs"),
      param.isSafe.option("safe"),
      param.hasDefault.option("optional"),
      param.isVariadic.option("variadic"),
      param.variadicAtLeastOne.option("at least one"),
      param.variadicFlatten.option("flatten")).flatten

}
