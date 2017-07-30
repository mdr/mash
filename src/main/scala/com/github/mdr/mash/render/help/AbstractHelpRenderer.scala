package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.screen.{ BasicColour, Line, Style, StyledString }
import com.github.mdr.mash.utils.LineInfo

abstract class AbstractHelpRenderer {

  protected val ParamNameStyle = Style(foregroundColour = BasicColour.Blue, bold = true)

  protected val FieldMethodStyle = Style(foregroundColour = BasicColour.Blue, bold = true)

  protected val SectionTitleStyle = Style(bold = true, foregroundColour = BasicColour.Yellow)

  protected val NameStyle = Style(bold = true)

  protected val IndentSpace = (" " * 4).style

  protected def renderNameSection(title: String, names: Seq[String], summaryOpt: Option[String]): Seq[Line] = {
    val titleLine = Line(SectionTitleStyle(title))
    val styledNames = ", ".style.join(names.map(NameStyle(_)))
    val namesLine = Line(IndentSpace + styledNames + summaryOpt.fold("")(" - " + _).style)
    Seq(titleLine, namesLine)
  }

  protected def renderDescriptionSection(descriptionOpt: Option[String]): Seq[Line] =
    descriptionOpt.toSeq.flatMap(description ⇒
      Seq(Line.Empty, Line(SectionTitleStyle("DESCRIPTION"))) ++ renderDescription(description))

  protected def renderDescription(description: String, indentLevel: Int = 1): Seq[Line] = {
    val renderedDescription: StyledString = MashMarkupRenderer.render(description)
    new LineInfo(renderedDescription.forgetStyling)
      .lineRegions
      .map(region ⇒ Line(IndentSpace * indentLevel + region.of(renderedDescription.chars)))
  }

  protected def renderClassSection(klass: MashClass): Seq[Line] =
    Seq(
      Line.Empty,
      Line(SectionTitleStyle("CLASS")),
      Line(IndentSpace + klass.fullyQualifiedName.toString.style))


}
