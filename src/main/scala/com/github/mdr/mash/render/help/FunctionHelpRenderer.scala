package com.github.mdr.mash.render.help

import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.render.{ CallingSyntaxRenderer, MashRenderer }
import com.github.mdr.mash.screen.{ Line, StyledString }
import com.github.mdr.mash.utils.LineInfo

object FunctionHelpRenderer extends AbstractHelpRenderer {

  def render(f: MashFunction): LinesAndLinks =
    LinesAndLinks.combine(Seq(
      renderNameSection(f),
      LinesAndLinks(renderCallingSyntaxSection(f)),
      LinesAndLinks(ParameterHelpRenderer.renderSection(f.params.params)),
      renderDescriptionSection(f.descriptionOpt),
      LinesAndLinks(renderSourceSection(f.sourceOpt))))

  private def renderNameSection(f: MashFunction): LinesAndLinks = {
    val names = (f.fullyQualifiedName +: f.aliases).map(_.toString)
    renderNameSection("FUNCTION", names, f.summaryOpt)
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
