package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.functions.{ MashFunction, MashMethod }
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.screen.Style._

object ClassHelpRenderer extends AbstractHelpRenderer {

  def render(klass: MashClass): Seq[Line] = {
    Seq(renderSummarySection(klass),
      renderDescriptionSection(klass.descriptionOpt),
      renderParentSection(klass),
      renderFieldSection(klass),
      renderStaticMethodSection(klass),
      renderMethodSection(klass)).flatten
  }

  private def renderSummarySection(klass: MashClass): Seq[Line] = {
    val summaryOpt = klass.summaryOpt
    val summaryLine = Line(IndentSpace + NameStyle(klass.fullyQualifiedName.toString) + summaryOpt.fold("")(" - " + _).style)
    Seq(
      Line(SectionTitleStyle("CLASS")),
      summaryLine)
  }

  private def renderParentSection(klass: MashClass): Seq[Line] =
    klass.parentOpt.toSeq.flatMap(parent ⇒
      Seq(
        Line.Empty,
        Line(SectionTitleStyle("PARENT")),
        Line(IndentSpace + parent.fullyQualifiedName.toString.style)))

  private def renderFieldSection(help: MashClass): Seq[Line] = {
    val fields = help.fields
    if (fields.nonEmpty) {
      val fieldLines =
        for (field ← fields)
          yield Line(IndentSpace + FieldMethodStyle(field.name) + field.summaryOpt.fold("")(" - " + _).style)
      Seq(Line.Empty, Line(SectionTitleStyle("FIELDS"))) ++ fieldLines
    } else
      Seq()
  }

  private def renderStaticMethodSection(help: MashClass): Seq[Line] = {
    val methods = help.staticMethods
    if (methods.nonEmpty) {
      Seq(Line.Empty, Line(SectionTitleStyle("STATIC METHODS"))) ++
        methods.map(renderMethodSummary)
    } else
      Seq()
  }

  private def renderMethodSection(help: MashClass): Seq[Line] = {
    val methods = help.methods
    if (methods.nonEmpty) {
      Seq(Line.Empty, Line(SectionTitleStyle("METHODS"))) ++
        methods.map(renderMethodSummary)
    } else
      Seq()
  }

  private def renderMethodSummary(method: MashMethod): Line =
    Line(IndentSpace + FieldMethodStyle(method.name) + method.summaryOpt.fold("")(" - " + _).style)

  private def renderMethodSummary(f: MashFunction): Line =
    Line(IndentSpace + FieldMethodStyle(f.name) + f.summaryOpt.fold("")(" - " + _).style)

}
