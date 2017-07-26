package com.github.mdr.mash.render.help

import com.github.mdr.mash.ns.core.help.FieldHelpClass
import com.github.mdr.mash.ns.core.help.FieldHelpClass.Wrapper
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.screen.Style._

object FieldHelpRenderer {

  import HelpRenderer._

  def render(obj: MashObject): Seq[Line] = {
    val help = FieldHelpClass.Wrapper(obj)
    Seq(
      renderNameSection(help),
      renderClassSection(help),
      renderDescriptionSection(help)).flatten
  }

  private def renderNameSection(help: Wrapper): Seq[Line] = {
    val nameLine = Line(IndentSpace + NameStyle(help.name) + help.summaryOpt.fold("")(" - " + _).style)
    Seq(
      Line(SectionTitleStyle("FIELD")),
      nameLine)
  }

  private def renderClassSection(help: Wrapper): Seq[Line] =
    Seq(
      Line.Empty,
      Line(SectionTitleStyle("CLASS")),
      Line(IndentSpace + help.klass.style))

  private def renderDescriptionSection(help: Wrapper): Seq[Line] =
    help.descriptionOpt.toSeq.flatMap(description ⇒
      Seq(Line.Empty,
        Line(SectionTitleStyle("DESCRIPTION"))
      ) ++ DescriptionRenderer.render(description))
}
