package com.github.mdr.mash.render.help

import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.core.help.HelpCreator
import com.github.mdr.mash.render.{ CallingSyntaxRenderer, MashRenderer }
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.screen.{ Line, StyledString }
import com.github.mdr.mash.utils.LineInfo

object FunctionHelpRenderer {

  import HelpRenderer._

  def render(f: MashFunction): Seq[Line] = {
    val paramHelp = f.params.params.map(HelpCreator.getParamHelp)
    Seq(
      renderNameSection(f),
      renderCallingSyntaxSection(f),
      ParameterHelpRenderer.renderSection(paramHelp),
      renderDescriptionSection(f.descriptionOpt),
      renderSourceSection(f.sourceOpt)).flatten
  }

  private def renderNameSection(f: MashFunction): Seq[Line] = {
    val names = (f.fullyQualifiedName +: f.aliases).map(_.toString)
    renderNameSection("FUNCTION", names, f.summaryOpt)
  }

  def renderNameSection(title: String, names: Seq[String], summaryOpt: Option[String]): Seq[Line] = {
    val titleLine = Line(SectionTitleStyle(title))
    val styledNames = ", ".style.join(names.map(NameStyle(_)))
    val namesLine = Line(IndentSpace + styledNames + summaryOpt.fold("")(" - " + _).style)
    Seq(titleLine, namesLine)
  }

  private def renderCallingSyntaxSection(f: MashFunction): Seq[Line] =
    if (f.params.isEmpty)
      Seq()
    else
      renderCallingSyntaxSection(CallingSyntaxRenderer.render(f))

  def renderCallingSyntaxSection(callingSyntax: StyledString): Seq[Line] =
    Seq(
      Line.Empty,
      Line(SectionTitleStyle("CALLING SYNTAX")),
      Line(IndentSpace + callingSyntax))

  def renderDescriptionSection(descriptionOpt: Option[String]): Seq[Line] =
    descriptionOpt.toSeq.flatMap(description ⇒
      Seq(Line.Empty, Line(SectionTitleStyle("DESCRIPTION"))) ++ DescriptionRenderer.render(description))

  def renderSourceSection(sourceOpt: Option[String]): Seq[Line] =
    sourceOpt.toSeq.flatMap(source ⇒
      Seq(Line.Empty, Line(SectionTitleStyle("SOURCE"))) ++ renderSource(source))

  private def renderSource(s: String): Seq[Line] = {
    val renderedSource = new MashRenderer().renderChars(s)
    new LineInfo(renderedSource.forgetStyling)
      .lineRegions
      .map(region ⇒ Line(IndentSpace + region.of(renderedSource.chars)))
  }

}
