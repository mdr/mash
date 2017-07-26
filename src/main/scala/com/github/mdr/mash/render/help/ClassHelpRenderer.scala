package com.github.mdr.mash.render.help

import com.github.mdr.mash.ns.core.help.{ ClassHelpClass, FunctionHelpClass }
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.screen.Style._

import scala.collection.mutable.ArrayBuffer

object ClassHelpRenderer {

  import HelpRenderer._

  private def renderMethodSummary(methodHelp: FunctionHelpClass.Wrapper): Line =
    Line(IndentSpace + FieldMethodStyle(methodHelp.name) + methodHelp.summaryOpt.fold("")(" - " + _).style)

  def renderClassHelp(obj: MashObject): Seq[Line] = {
    val lines = ArrayBuffer[Line]()

    val classHelp = ClassHelpClass.Wrapper(obj)
    lines += Line(SectionTitleStyle("CLASS"))
    val summaryOpt = classHelp.summaryOpt
    lines += Line(IndentSpace + NameStyle(classHelp.fullyQualifiedName) + summaryOpt.fold("")(" - " + _).style)
    for (description ← classHelp.descriptionOpt) {
      lines += Line.Empty
      lines += Line(SectionTitleStyle("DESCRIPTION"))
      lines ++= DescriptionRenderer.renderDescription(description)
    }
    for (parent ← classHelp.parentOpt) {
      lines += Line.Empty
      lines += Line(SectionTitleStyle("PARENT"))
      lines += Line(IndentSpace + parent.style)
    }
    val fields = classHelp.fields
    if (fields.nonEmpty) {
      lines += Line.Empty
      lines += Line(SectionTitleStyle("FIELDS"))
      for (field ← fields)
        lines += Line(IndentSpace + FieldMethodStyle(field.name) + field.summaryOpt.fold("")(" - " + _).style)
    }
    val staticMethods = classHelp.staticMethods
    if (staticMethods.nonEmpty) {
      lines += Line.Empty
      lines += Line(SectionTitleStyle("STATIC METHODS"))
      lines ++= staticMethods.map(renderMethodSummary)
    }
    val methods = classHelp.methods
    if (methods.nonEmpty) {
      lines += Line.Empty
      lines += Line(SectionTitleStyle("METHODS"))
      lines ++= methods.map(renderMethodSummary)
    }

    lines
  }

}
