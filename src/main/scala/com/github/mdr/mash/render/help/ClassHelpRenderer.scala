package com.github.mdr.mash.render.help

import com.github.mdr.mash.ns.core.help.ClassHelpClass.Wrapper
import com.github.mdr.mash.ns.core.help.{ ClassHelpClass, FunctionHelpClass }
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.screen.Style._

import scala.collection.mutable.ArrayBuffer

object ClassHelpRenderer {

  import HelpRenderer._

  def render(obj: MashObject): Seq[Line] = {
    val help = ClassHelpClass.Wrapper(obj)
    Seq(renderSummarySection(help),
      renderDescriptionSection(help),
      renderParentSection(help),
      renderFieldSection(help),
      renderStaticMethodSection(help),
      renderMethodSection(help)).flatten
  }

  private def renderSummarySection(help: Wrapper): Seq[Line] = {
    val summaryOpt = help.summaryOpt
    val summaryLine = Line(IndentSpace + NameStyle(help.fullyQualifiedName) + summaryOpt.fold("")(" - " + _).style)
    Seq(
      Line(SectionTitleStyle("CLASS")),
      summaryLine)
  }

  private def renderDescriptionSection(help: Wrapper): Seq[Line] =
    help.descriptionOpt.toSeq.flatMap(description ⇒
      Seq(Line.Empty, Line(SectionTitleStyle("DESCRIPTION"))) ++ DescriptionRenderer.render(description))

  private def renderParentSection(help: Wrapper): Seq[Line] =
    help.parentOpt.toSeq.flatMap(parent ⇒
      Seq(
        Line.Empty,
        Line(SectionTitleStyle("PARENT")),
        Line(IndentSpace + parent.style)))

  private def renderFieldSection(help: Wrapper): Seq[Line] = {
    val fields = help.fields
    if (fields.nonEmpty) {
      val fieldLines =
        for (field ← fields)
          yield Line(IndentSpace + FieldMethodStyle(field.name) + field.summaryOpt.fold("")(" - " + _).style)
      Seq(Line.Empty, Line(SectionTitleStyle("FIELDS"))) ++ fieldLines
    } else
      Seq()
  }

  private def renderStaticMethodSection(help: Wrapper): Seq[Line] = {
    val methods = help.staticMethods
    if (methods.nonEmpty) {
      Seq(Line.Empty, Line(SectionTitleStyle("STATIC METHODS"))) ++
        methods.map(renderMethodSummary)
    } else
      Seq()
  }

  private def renderMethodSection(help: Wrapper): Seq[Line] = {
    val methods = help.methods
    if (methods.nonEmpty) {
      Seq(Line.Empty, Line(SectionTitleStyle("METHODS"))) ++
        methods.map(renderMethodSummary)
    } else
      Seq()
  }

  private def renderMethodSummary(methodHelp: FunctionHelpClass.Wrapper): Line =
    Line(IndentSpace + FieldMethodStyle(methodHelp.name) + methodHelp.summaryOpt.fold("")(" - " + _).style)

}
