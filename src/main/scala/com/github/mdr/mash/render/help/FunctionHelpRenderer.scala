package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.{ BoundMethod, MashClass, UserDefinedMethod }
import com.github.mdr.mash.functions.{ MashFunction, MashMethod }
import com.github.mdr.mash.ns.core.help.{ FunctionHelpClass, HelpCreator, MethodHelpClass }
import com.github.mdr.mash.render.{ CallingSyntaxRenderer, MashRenderer }
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.screen.{ Line, StyledString }
import com.github.mdr.mash.utils.LineInfo

object FunctionHelpRenderer {

  import HelpRenderer._

  def renderMethod(obj: MashObject): Seq[Line] = {
    val help = MethodHelpClass.Wrapper(obj)
    val method = help.method
    val paramHelp = method.params.params.map(HelpCreator.getParamHelp)
    Seq(
      renderNameSection(method),
      renderClassSection(help.klass),
      renderCallingSyntaxSection(CallingSyntaxRenderer.render(method)),
      ParameterHelpRenderer.renderSection(paramHelp),
      renderDescriptionSection(method.descriptionOpt),
      renderSourceSection(getSource(method))).flatten
  }

  private def getSource(method: MashMethod): Option[String] = method match {
    case udm: UserDefinedMethod ⇒ udm.sourceLocationOpt.map(_.source)
    case _                      ⇒ None
  }

  private def renderClassSection(klass: MashClass): Seq[Line] =
    Seq(
      Line.Empty,
      Line(SectionTitleStyle("CLASS")),
      Line(IndentSpace + klass.fullyQualifiedName.toString.style))

  private def renderNameSection(method: MashMethod): Seq[Line] = {
    val names = method.name +: method.aliases
    renderNameSection("METHOD", names, method.summaryOpt)
  }

  def render(obj: MashObject): Seq[Line] = {
    val help = FunctionHelpClass.Wrapper(obj)
    Seq(
      renderNameSection(help),
      renderCallingSyntaxSection(help),
      ParameterHelpRenderer.renderSection(help),
      renderDescriptionSection(help.descriptionOpt),
      renderSourceSection(help.sourceOpt)).flatten
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
    }.get

  private def renderCallingSyntaxSection(help: FunctionHelpClass.Wrapper): Seq[Line] =
    help.functionOpt
      .collect {
        case f: MashFunction if f.params.nonEmpty  ⇒ CallingSyntaxRenderer.render(f)
        case bm: BoundMethod if bm.params.nonEmpty ⇒ CallingSyntaxRenderer.render(bm)
      }
      .map(callingSyntax ⇒
        renderCallingSyntaxSection(callingSyntax))
      .getOrElse(Seq())

  private def renderCallingSyntaxSection(callingSyntax: StyledString): Seq[Line] =
    Seq(
      Line.Empty,
      Line(SectionTitleStyle("CALLING SYNTAX")),
      Line(IndentSpace + callingSyntax))

  private def renderDescriptionSection(descriptionOpt: Option[String]): Seq[Line] =
    descriptionOpt.toSeq.flatMap(description ⇒
      Seq(Line.Empty, Line(SectionTitleStyle("DESCRIPTION"))) ++ DescriptionRenderer.render(description))

  private def renderSourceSection(sourceOpt: Option[String]): Seq[Line] =
    sourceOpt.toSeq.flatMap(source ⇒
      Seq(Line.Empty, Line(SectionTitleStyle("SOURCE"))) ++ renderSource(source))

  private def renderSource(s: String): Seq[Line] = {
    val renderedSource = new MashRenderer().renderChars(s)
    new LineInfo(renderedSource.forgetStyling)
      .lineRegions
      .map(region ⇒ Line(IndentSpace + region.of(renderedSource.chars)))
  }

}
