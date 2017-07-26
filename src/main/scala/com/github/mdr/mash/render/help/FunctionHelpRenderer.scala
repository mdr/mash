package com.github.mdr.mash.render.help

import com.github.mdr.mash.ns.core.help.FunctionHelpClass
import com.github.mdr.mash.render.MashRenderer
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.utils.LineInfo

object FunctionHelpRenderer {

  import HelpRenderer._

  def render(obj: MashObject): Seq[Line] = {
    val help = FunctionHelpClass.Wrapper(obj)
    Seq(
      renderNameSection(help),
      renderClassSection(help),
      renderCallingSyntaxSection(help),
      ParameterHelpRenderer.renderSection(help),
      renderDescriptionSection(help),
      renderSourceSection(help)).flatten
  }

  private def renderNameSection(help: FunctionHelpClass.Wrapper): Seq[Line] = {
    val titleLine = Line(SectionTitleStyle(if (help.classOpt.isDefined) "METHOD" else "FUNCTION"))
    val names = help.fullyQualifiedName +: help.aliases
    val styledNames = ", ".style.join(names.map(NameStyle(_)))
    val namesLine = Line(IndentSpace + styledNames + help.summaryOpt.fold("")(" - " + _).style)
    Seq(titleLine, namesLine)
  }

  private def renderClassSection(help: FunctionHelpClass.Wrapper): Seq[Line] =
    help.classOpt.toSeq.flatMap(klass ⇒
      Seq(
        Line.Empty,
        Line(SectionTitleStyle("CLASS")),
        Line(IndentSpace + klass.style)))

  private def renderCallingSyntaxSection(help: FunctionHelpClass.Wrapper): Seq[Line] = {
    val maybeTarget = if (help.classOpt.isDefined) "target." else ""
    Seq(
      Line.Empty,
      Line(SectionTitleStyle("CALLING SYNTAX")),
      Line(IndentSpace + maybeTarget.style + help.callingSyntax.style))
  }

  private def renderDescriptionSection(help: FunctionHelpClass.Wrapper): Seq[Line] =
    help.descriptionOpt.toSeq.flatMap(description ⇒
      Seq(Line.Empty, Line(SectionTitleStyle("DESCRIPTION"))) ++ DescriptionRenderer.render(description))

  private def renderSourceSection(help: FunctionHelpClass.Wrapper): Seq[Line] =
    help.sourceOpt.toSeq.flatMap(source ⇒
      Seq(Line.Empty, Line(SectionTitleStyle("SOURCE"))) ++ renderSource(source))

  private def renderSource(s: String): Seq[Line] = {
    val renderedSource = new MashRenderer().renderChars(s)
    new LineInfo(renderedSource.forgetStyling)
      .lineRegions
      .map(region ⇒ Line(IndentSpace + region.of(renderedSource.chars)))
  }

}
