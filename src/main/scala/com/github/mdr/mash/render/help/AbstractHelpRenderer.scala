package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.printer.model.Link
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.screen.{ BasicColour, Line, Style, StyledString }
import com.github.mdr.mash.utils.{ LineInfo, Region }

object LinesAndLinks {

  val Empty = LinesAndLinks()

  def apply(line: Line, link: Link): LinesAndLinks = LinesAndLinks(Seq(line), Seq(link))

  def combine(linesAndLinks: Seq[LinesAndLinks]): LinesAndLinks =
    linesAndLinks.fold(LinesAndLinks.Empty)(_ combine _)

}

case class LinesAndLinks(lines: Seq[Line] = Seq(), links: Seq[Link] = Seq()) {

  def combine(that: LinesAndLinks): LinesAndLinks = {
    def updateLink(link: Link) = link.copy(line = link.line + this.lines.size)
    val combinedLines = this.lines ++ that.lines
    val combinedLinks = this.links ++ that.links.map(updateLink)
    LinesAndLinks(combinedLines, combinedLinks)
  }

}

abstract class AbstractHelpRenderer {

  protected val ParamNameStyle = Style(foregroundColour = BasicColour.Blue, bold = true)

  protected val FieldMethodStyle = Style(foregroundColour = BasicColour.Blue, bold = true)

  protected val SectionTitleStyle = Style(bold = true, foregroundColour = BasicColour.Yellow)

  protected val NameStyle = Style(bold = true)

  protected val IndentSpace = (" " * 4).style

  protected def renderNameSection(title: String,
                                  names: Seq[String],
                                  summaryOpt: Option[String]): LinesAndLinks = {
    val titleLine = Line(SectionTitleStyle(title))
    val styledNames = ", ".style.join(names.map(NameStyle(_)))
    val namesLine = Line(IndentSpace + styledNames + summaryOpt.fold("")(" - " + _).style)
    LinesAndLinks(Seq(titleLine, namesLine))
  }

  protected def renderDescriptionSection(descriptionOpt: Option[String]): LinesAndLinks =
    LinesAndLinks(
      descriptionOpt.toSeq.flatMap(description ⇒
        Seq(Line.Empty, Line(SectionTitleStyle("DESCRIPTION"))) ++ renderDescription(description)))

  protected def renderDescription(description: String, indentLevel: Int = 1): Seq[Line] = {
    val renderedDescription: StyledString = MashMarkupRenderer.render(description)
    new LineInfo(renderedDescription.forgetStyling)
      .lineRegions
      .map(region ⇒ Line(IndentSpace * indentLevel + region.of(renderedDescription.chars)))
  }

  protected def renderClassSection(klass: MashClass): LinesAndLinks = {
    val renderedParent = klass.fullyQualifiedName.toString.style
    val lines =
      Seq(
        Line.Empty,
        Line(SectionTitleStyle("CLASS")),
        Line(IndentSpace + renderedParent))
    val parentLink = Link(2, Region(IndentSpace.length, renderedParent.length), klass)
    LinesAndLinks(lines, Seq(parentLink))
  }

}
