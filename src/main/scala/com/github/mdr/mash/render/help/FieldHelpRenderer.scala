package com.github.mdr.mash.render.help

import com.github.mdr.mash.ns.core.help.FieldHelpClass
import com.github.mdr.mash.ns.core.help.FieldHelpClass.Wrapper
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.screen.Style._

object FieldHelpRenderer {

  import HelpRenderer._

  def render(obj: MashObject): Seq[Line] = {
    val fieldHelp = FieldHelpClass.Wrapper(obj)
    Seq(
      renderNameSection(fieldHelp),
      renderClassSection(fieldHelp),
      renderDescriptionSection(fieldHelp)).flatten
  }

  private def renderNameSection(fieldHelp: Wrapper): Seq[Line] = {
    val nameLine = Line(IndentSpace + NameStyle(fieldHelp.name) + fieldHelp.summaryOpt.fold("")(" - " + _).style)
    Seq(
      Line(SectionTitleStyle("FIELD")),
      nameLine)
  }

  private def renderClassSection(fieldHelp: Wrapper): Seq[Line] =
    Seq(
      Line.Empty,
      Line(SectionTitleStyle("CLASS")),
      Line(IndentSpace + fieldHelp.klass.style))

  private def renderDescriptionSection(fieldHelp: Wrapper): Seq[Line] =
    fieldHelp.descriptionOpt.toSeq.flatMap(description ⇒
      Seq(Line.Empty,
        Line(SectionTitleStyle("DESCRIPTION"))
      ) ++ DescriptionRenderer.render(description))
}
