package com.github.mdr.mash.render

import java.util.regex.Pattern

import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.ns.core.help.{ ClassHelpClass, FieldHelpClass, FunctionHelpClass, ParameterHelpClass }
import com.github.mdr.mash.runtime.{ MashObject, MashString }
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.screen.{ BasicColour, Line, Style, StyledString }
import com.github.mdr.mash.utils.{ LineInfo, Region }
import com.github.mdr.mash.utils.StringUtils._

import scala.collection.mutable.ArrayBuffer

/**
  * Render function/method/field help objects in a similar style to man pages
  */
object HelpRenderer {

  private def paramNameStyle(s: String) = s.style(Style(foregroundColour = BasicColour.Blue, bold = true))

  private def fieldMethodStyle(s: String) = s.style(Style(foregroundColour = BasicColour.Blue, bold = true))

  private def bold(s: String) = s.style(Style(bold = true))

  private val indentAmount = 4

  private val indentSpace = " " * indentAmount

  def renderFunctionHelp(obj: MashObject): Seq[Line] = {
    import FunctionHelpClass.Fields._
    val help = FunctionHelpClass.Wrapper(obj)
    val lines = ArrayBuffer[Line]()
    lines += Line(bold(if (help.classOpt.isDefined) "METHOD" else "FUNCTION"))
    val name = help.fullyQualifiedName
    val names = ", ".style.join((name +: help.aliases).map(bold(_)))
    lines += Line(indentSpace.style + names + help.summaryOpt.fold("")(" - " + _).style)
    lines += Line.Empty
    for (klass ← help.classOpt) {
      lines += Line(bold("CLASS"))
      lines += Line((indentSpace + klass).style)
      lines += Line.Empty
    }
    lines += Line(bold("CALLING SYNTAX"))
    val maybeTarget = if (help.classOpt.isDefined) "target." else ""
    lines += Line((indentSpace + maybeTarget + obj(CallingSyntax)).style)
    lines += Line.Empty
    val parameters = help.parameters
    if (parameters.nonEmpty) {
      lines += Line(bold("PARAMETERS"))

      for (param ← parameters)
        lines ++= renderParameterHelp(param.asInstanceOf[MashObject])
    }
    for (description ← help.descriptionOpt) {
      lines += Line(bold("DESCRIPTION"))
      lines ++= renderDescription(description)
      lines += Line.Empty
    }
    lines
  }

  private def renderParameterHelp(param: MashObject): Seq[Line] = {
    val lines = ArrayBuffer[Line]()
    val paramHelp = ParameterHelpClass.Wrapper(param)
    var qualifiers: Seq[String] = Seq()
    val isFlag = paramHelp.isFlag
    if (paramHelp.isLazy)
      qualifiers +:= "lazy"
    if (paramHelp.isNamedArgs)
      qualifiers +:= "namedArgs"
    if (paramHelp.isOptional)
      qualifiers +:= "optional"
    if (paramHelp.isVariadic)
      qualifiers +:= "variadic"
    val qualifierString = qualifiers match {
      case Seq() ⇒ ""
      case _     ⇒ qualifiers.mkString(" [", ", ", "]")
    }
    val name = paramHelp.nameOpt getOrElse Parameter.AnonymousParamName
    val paramName = paramNameStyle(if (isFlag) "--" + name else name)
    val shortFlagDescription = paramNameStyle(paramHelp.shortFlagOpt.map(f ⇒ s" | -$f").getOrElse(""))
    lines += Line(indentSpace.style + paramName + shortFlagDescription + qualifierString.style + paramHelp.summaryOpt.fold("")(" - " + _).style)
    for (description ← paramHelp.descriptionOpt)
      lines += Line(indent(description, indentAmount * 2).style)
    lines += Line.Empty
    lines
  }

  def renderFieldHelp(obj: MashObject): Seq[Line] = {
    val lines = ArrayBuffer[Line]()
    val fieldHelp = FieldHelpClass.Wrapper(obj)
    lines += Line(bold("FIELD"))
    lines += Line(indentSpace.style + bold(fieldHelp.name) + fieldHelp.summaryOpt.fold("")(" - " + _).style)
    lines += Line.Empty
    lines += Line(bold("CLASS"))
    lines += Line((indentSpace + fieldHelp.klass).style)
    lines += Line.Empty
    for (description ← fieldHelp.descriptionOpt) {
      lines += Line(bold("DESCRIPTION"))
      lines += Line(indent(description, indentAmount).style)
      lines += Line.Empty
    }
    lines
  }

  private def renderMethodSummary(methodHelp: FunctionHelpClass.Wrapper): Line =
    Line(indentSpace.style + fieldMethodStyle(methodHelp.name) + methodHelp.summaryOpt.fold("")(" - " + _).style)

  def renderClassHelp(obj: MashObject): Seq[Line] = {
    val lines = ArrayBuffer[Line]()

    val classHelp = ClassHelpClass.Wrapper(obj)
    lines += Line(bold("CLASS"))
    val summaryOpt = classHelp.summaryOpt
    lines += Line(indentSpace.style + bold(classHelp.fullyQualifiedName) + summaryOpt.fold("")(" - " + _).style)
    for (description ← classHelp.descriptionOpt.map(_.asInstanceOf[MashString])) {
      lines += Line.Empty
      lines += Line(bold("DESCRIPTION"))
      lines += Line(indent(description.s, indentAmount).style)
    }
    for (parent ← classHelp.parentOpt) {
      lines += Line.Empty
      lines += Line(bold("PARENT"))
      lines += Line((indentSpace + parent).style)
    }
    val fields = classHelp.fields
    if (fields.nonEmpty) {
      lines += Line.Empty
      lines += Line(bold("FIELDS"))
      for (field ← fields)
        lines += Line(indentSpace.style + fieldMethodStyle(field.name) + field.summaryOpt.fold("")(" - " + _).style)
    }
    val staticMethods = classHelp.staticMethods
    if (staticMethods.nonEmpty) {
      lines += Line.Empty
      lines += Line(bold("STATIC METHODS"))
      lines ++= staticMethods.map(renderMethodSummary)
    }
    val methods = classHelp.methods
    if (methods.nonEmpty) {
      lines += Line.Empty
      lines += Line(bold("METHODS"))
      lines ++= methods.map(renderMethodSummary)
    }

    lines
  }

  private val MashMarkupPattern = Pattern.compile("<mash>(.+?)</mash>", Pattern.DOTALL)

  private def renderDescription(s: String): Seq[Line] = {
    val matcher = MashMarkupPattern.matcher(s)
    val chunks = ArrayBuffer[StyledString]()
    var previous = 0
    while (matcher.find()) {
      val region = Region.fromStartEnd(previous, matcher.start)
      chunks += region.of(s).style
      previous = matcher.end
      val contents = matcher.group(1)
      val renderedMash = new MashRenderer().renderChars(trimInitialAndFinalNewlines(contents))
      chunks += renderedMash
    }
    chunks += s.substring(previous).style
    val renderedDescription = StyledString.empty.join(chunks)
    new LineInfo(renderedDescription.forgetStyling)
      .lineRegions
      .map(region ⇒ Line(indentSpace.style + region.of(renderedDescription.chars)))
  }

  private def trimInitialAndFinalNewlines(contents: String): String = {
    var inner = contents
    if (inner startsWith "\r")
      inner = inner.tail
    if (inner startsWith "\n")
      inner = inner.tail
    if (inner endsWith "\n")
      inner = inner.init
    if (inner endsWith "\r")
      inner = inner.init
    inner
  }

}
