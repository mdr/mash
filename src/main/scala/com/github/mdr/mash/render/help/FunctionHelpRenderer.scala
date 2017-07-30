package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.BoundMethod
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.core.help.FunctionHelpClass
import com.github.mdr.mash.render.{ CallingSyntaxRenderer, MashRenderer }
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

  private def renderNameSection(f: MashFunction): Seq[Line] = {
    val names = (f.fullyQualifiedName +: f.aliases).map(_.toString)
    renderNameSection("FUNCTION", names, f.summaryOpt)
  }

  private def renderNameSection(title: String, names: Seq[String], summaryOpt: Option[String]): Seq[Line] = {
    val titleLine = Line(SectionTitleStyle(title))
    val styledNames = ", ".style.join(names.map(NameStyle(_)))
    val namesLine = Line(IndentSpace + styledNames + summaryOpt.fold("")(" - " + _).style)
    Seq(titleLine, namesLine)
  }

  private def renderNameSection(help: FunctionHelpClass.Wrapper): Seq[Line] =
    help.functionOpt.collect {
      case f: MashFunction ⇒ renderNameSection(f)
    }.getOrElse {
      val names = help.fullyQualifiedName +: help.aliases
      renderNameSection("METHOD", names, help.summaryOpt)
    }

  private def renderClassSection(help: FunctionHelpClass.Wrapper): Seq[Line] =
    help.classOpt.toSeq.flatMap(klass ⇒
      Seq(
        Line.Empty,
        Line(SectionTitleStyle("CLASS")),
        Line(IndentSpace + klass.style)))

  private def renderCallingSyntaxSection(help: FunctionHelpClass.Wrapper): Seq[Line] =
    help.functionOpt
      .collect {
        case f: MashFunction if f.params.nonEmpty  ⇒ CallingSyntaxRenderer.render(f)
        case bm: BoundMethod if bm.params.nonEmpty ⇒ CallingSyntaxRenderer.render(bm)
      }
      .map(callingSyntax ⇒
        Seq(
          Line.Empty,
          Line(SectionTitleStyle("CALLING SYNTAX")),
          Line(IndentSpace + callingSyntax)))
      .getOrElse(Seq())

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
