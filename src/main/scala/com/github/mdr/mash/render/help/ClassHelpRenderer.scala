package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.functions.{ MashFunction, MashMethod }
import com.github.mdr.mash.ns.core.help.{ FieldHelpClass, MethodHelpClass }
import com.github.mdr.mash.printer.model.{ Link, LinkPath }
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
        val linkPath = LinkPath.Absolute(parent.fullyQualifiedName.toString)
        val parentLink = Link(line = 2, Region(IndentSpace.length, renderedParent.length), parent, linkPath)
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
    val line = Line(IndentSpace + FieldAndMethodStyle(field.name) + field.summaryOpt.fold("")(" - " + _).style)
    val linkPath = LinkPath.Relative(s""".helpForField "${field.name}"""")
    val fieldLink = Link(line = 0, Region(IndentSpace.length, field.name.length), FieldHelpClass.create(field.name, klass), linkPath)
    LinesAndLinks(line, fieldLink)
  }

  private def renderStaticMethodSection(klass: MashClass): LinesAndLinks = {
    val methods = klass.staticMethods.sortBy(_.name)
    if (methods.nonEmpty) {
      LinesAndLinks.combine(
        LinesAndLinks(Seq(Line.Empty, Line(SectionTitleStyle("STATIC METHODS")))) +:
          methods.map(renderStaticMethodSummary(_)))
    } else
      LinesAndLinks.Empty
  }

  private def renderMethodSection(klass: MashClass): LinesAndLinks = {
    val methods = klass.methods.filterNot(_.isPrivate).sortBy(_.name)
    if (methods.nonEmpty) {
      val titleLines = LinesAndLinks(Seq(
        Line.Empty,
        Line(SectionTitleStyle("METHODS"))))
      LinesAndLinks.combine(titleLines +: methods.map(renderMethodSummary(_, klass)))
    } else
      LinesAndLinks.Empty
  }

  private def renderMethodSummary(method: MashMethod, klass: MashClass): LinesAndLinks = {
    val line = Line(IndentSpace + FieldAndMethodStyle(method.name) + method.summaryOpt.fold("")(" - " + _).style)
    val linkPath = LinkPath.Relative(s""".helpForMethod "${method.name}"""")
    val methodHelp = MethodHelpClass.create(method.name, klass)
    val methodLink = Link(line = 0, Region(IndentSpace.length, method.name.length), methodHelp, linkPath)
    LinesAndLinks(line, methodLink)
  }

  private def renderStaticMethodSummary(f: MashFunction): LinesAndLinks = {
    val name = f.name
    val line = Line(IndentSpace + FieldAndMethodStyle(name) + f.summaryOpt.fold("")(" - " + _).style)
    val pathFragment = if (f.allowsNullary) s"""["$name"]""" else s".$name"
    val methodLink = Link(line = 0, Region(IndentSpace.length, name.length), f, LinkPath.Relative(pathFragment))
    LinesAndLinks(line, methodLink)
  }

}
