package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.functions.{ MashFunction, MashMethod }
import com.github.mdr.mash.ns.core.help.{ FieldHelpClass, MethodHelpClass }
import com.github.mdr.mash.printer.model.Link
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.utils.Region

object ClassHelpRenderer extends AbstractHelpRenderer {

  def render(klass: MashClass): LinesAndLinks =
    LinesAndLinks.combine(Seq(
      LinesAndLinks(renderSummarySection(klass)),
      renderDescriptionSection(klass.descriptionOpt),
      renderParentSection(klass),
      renderFieldSection(klass),
      renderStaticMethodSection(klass),
      renderMethodSection(klass)))

  private def renderSummarySection(klass: MashClass): Seq[Line] = {
    val summaryOpt = klass.summaryOpt
    val summaryLine = Line(IndentSpace + NameStyle(klass.fullyQualifiedName.toString) + summaryOpt.fold("")(" - " + _).style)
    Seq(
      Line(SectionTitleStyle("CLASS")),
      summaryLine)
  }

  private def renderParentSection(klass: MashClass): LinesAndLinks =
    klass.parentOpt match {
      case Some(parent) ⇒
        val renderedParent = parent.fullyQualifiedName.toString.style
        val lines =
          Seq(
            Line.Empty,
            Line(SectionTitleStyle("PARENT")),
            Line(IndentSpace + renderedParent))
        val parentLink = Link(2, Region(IndentSpace.length, renderedParent.length), parent, ".parent")
        LinesAndLinks(lines, Seq(parentLink))
      case None         ⇒
        LinesAndLinks.Empty
    }

  private def renderFieldSection(klass: MashClass): LinesAndLinks = {
    val fields = klass.fields
    if (fields.nonEmpty) {
      LinesAndLinks.combine(
        LinesAndLinks(Seq(Line.Empty, Line(SectionTitleStyle("FIELDS")))) +:
          fields.map(renderField(_, klass)))
    } else
      LinesAndLinks.Empty
  }

  private def renderField(field: Field, klass: MashClass): LinesAndLinks = {
    val line = Line(IndentSpace + FieldMethodStyle(field.name) + field.summaryOpt.fold("")(" - " + _).style)
    val fieldLink = Link(0, Region(IndentSpace.length, field.name.length), FieldHelpClass.create(field.name, klass), s""".helpForField "${field.name}"""")
    LinesAndLinks(line, fieldLink)
  }

  private def renderStaticMethodSection(klass: MashClass): LinesAndLinks = {
    val methods = klass.staticMethods.sortBy(_.name)
    if (methods.nonEmpty) {
      LinesAndLinks.combine(
        LinesAndLinks(Seq(Line.Empty, Line(SectionTitleStyle("STATIC METHODS")))) +:
          methods.map(renderMethodSummary(_)))
    } else
      LinesAndLinks.Empty
  }

  private def renderMethodSection(klass: MashClass): LinesAndLinks = {
    val methods = klass.methods.sortBy(_.name)
    if (methods.nonEmpty) {
      LinesAndLinks.combine(
        LinesAndLinks(Seq(Line.Empty, Line(SectionTitleStyle("METHODS")))) +:
          methods.map(renderMethodSummary(_, klass)))
    } else
      LinesAndLinks.Empty
  }

  private def renderMethodSummary(method: MashMethod, klass: MashClass): LinesAndLinks = {
    val line = Line(IndentSpace + FieldMethodStyle(method.name) + method.summaryOpt.fold("")(" - " + _).style)
    val methodLink = Link(0, Region(IndentSpace.length, method.name.length), MethodHelpClass.create(method.name, klass), s""".helpForMethod "${method.name}"""")
    LinesAndLinks(line, methodLink)
  }

  private def renderMethodSummary(f: MashFunction): LinesAndLinks = {
    val line = Line(IndentSpace + FieldMethodStyle(f.name) + f.summaryOpt.fold("")(" - " + _).style)
    val methodLink = Link(0, Region(IndentSpace.length, f.name.length), f, ".todo")
    LinesAndLinks(line, methodLink)
  }

}
