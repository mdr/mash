package com.github.mdr.mash.render.help

import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.ns.core.help.{ FunctionHelpClass, ParameterHelpClass }
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.utils.Utils._

object ParameterHelpRenderer {

  import HelpRenderer._

  def renderSection(help: FunctionHelpClass.Wrapper): Seq[Line] = {
    val parameters = help.parameters
    if (parameters.nonEmpty) {
      val headingLines = Seq(Line.Empty, Line(SectionTitleStyle("PARAMETERS")))
      val paramLines = parameters.zipWithIndex.flatMap { case (param, i) ⇒
        val paramHelp = ParameterHelpClass.Wrapper(param)
        val paddingLines = if (i > 0) Seq(Line.Empty) else Seq()
        paddingLines ++ renderParameterHelp(paramHelp)
      }
      headingLines ++ paramLines
    } else
      Seq()
  }

  private def renderParameterHelp(paramHelp: ParameterHelpClass.Wrapper): Seq[Line] = {
    val qualifierString = getParamQualifiers(paramHelp) match {
      case Seq()      ⇒ ""
      case qualifiers ⇒ qualifiers.mkString(" [", ", ", "]")
    }
    val name = paramHelp.nameOpt getOrElse Parameter.AnonymousParamName
    val paramName = ParamNameStyle(if (paramHelp.isFlag) "--" + name else name)
    val shortFlagDescription = paramHelp.shortFlagOpt.map(f ⇒ s" | -$f").getOrElse("").style(ParamNameStyle)
    val summaryDescription = paramHelp.summaryOpt.fold("")(" - " + _).style
    val summaryLine = Line(IndentSpace + paramName + shortFlagDescription + qualifierString.style + summaryDescription)
    val descriptionLines = paramHelp.descriptionOpt.toSeq.flatMap(description ⇒
      DescriptionRenderer.render(description, indentLevel = 2))
    summaryLine +: descriptionLines
  }

  private def getParamQualifiers(paramHelp: ParameterHelpClass.Wrapper): Seq[String] =
    Seq(
      paramHelp.isLazy.option("lazy"),
      paramHelp.isNamedArgs.option("namedArgs"),
      paramHelp.isOptional.option("optional"),
      paramHelp.isVariadic.option("variadic")).flatten

}
