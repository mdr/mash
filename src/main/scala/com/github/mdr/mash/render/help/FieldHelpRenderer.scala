package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.ns.core.help.FieldHelpClass
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.screen.Style._

object FieldHelpRenderer {

  import HelpRenderer._

  def render(obj: MashObject): Seq[Line] = {
    val help = FieldHelpClass.Wrapper(obj)
    val field = help.field
    Seq(
      renderNameSection(field),
      renderClassSection(help.klass),
      renderDescriptionSection(field)).flatten
  }

  private def renderNameSection(field: Field): Seq[Line] = {
    val nameLine = Line(IndentSpace + NameStyle(field.name) + field.summaryOpt.fold("")(" - " + _).style)
    Seq(
      Line(SectionTitleStyle("FIELD")),
      nameLine)
  }

  private def renderClassSection(klass: MashClass): Seq[Line] =
    Seq(
      Line.Empty,
      Line(SectionTitleStyle("CLASS")),
      Line(IndentSpace + klass.fullyQualifiedName.toString.style))

  private def renderDescriptionSection(field: Field): Seq[Line] =
    field.descriptionOpt.toSeq.flatMap(description ⇒
      Seq(Line.Empty,
        Line(SectionTitleStyle("DESCRIPTION"))
      ) ++ DescriptionRenderer.render(description))
}
