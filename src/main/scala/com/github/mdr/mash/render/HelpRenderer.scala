package com.github.mdr.mash.render

import java.util.regex.Pattern

import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.ns.core.help.{ ClassHelpClass, FieldHelpClass, FunctionHelpClass, ParameterHelpClass }
import com.github.mdr.mash.runtime.{ MashObject, MashString }
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.screen.{ BasicColour, Line, Style, StyledString }
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils._

import scala.collection.mutable.ArrayBuffer

/**
  * Render function/method/field help objects in a similar style to man pages
  */
object HelpRenderer {

  private def paramNameStyle(s: String) = s.style(Style(foregroundColour = BasicColour.Blue, bold = true))

  private def fieldMethodStyle(s: String) = s.style(Style(foregroundColour = BasicColour.Blue, bold = true))

  private def bold(s: String) = s.style(Style(bold = true))

  private val indentAmount = 8

  private val indentSpace = " " * indentAmount

  def renderFunctionHelp(obj: MashObject): Seq[Line] = {
    import FunctionHelpClass.Fields._
    val help = FunctionHelpClass.Wrapper(obj)
    val lines = ArrayBuffer[Line]()
    lines.append(Line(bold(if (help.classOpt.isDefined) "METHOD" else "FUNCTION")))
    val name = help.fullyQualifiedName
    val names = ", ".style.join((name +: help.aliases).map(bold(_)))
    lines.append(Line(indentSpace.style + names + help.summaryOpt.fold("")(" - " + _).style))
    lines.append(Line.Empty)
    for (klass ← help.classOpt) {
      lines.append(Line(bold("CLASS")))
      lines.append(Line((indentSpace + klass).style))
      lines.append(Line.Empty)
    }
    lines.append(Line(bold("CALLING SYNTAX")))
    val maybeTarget = if (help.classOpt.isDefined) "target." else ""
    lines.append(Line((indentSpace + maybeTarget + obj(CallingSyntax)).style))
    lines.append(Line.Empty)
    val parameters = help.parameters
    if (parameters.nonEmpty) {
      lines.append(Line(bold("PARAMETERS")))

      for (param ← parameters)
        lines.appendAll(renderParameterHelp(param.asInstanceOf[MashObject]))
    }
    for (description ← help.descriptionOpt) {
      lines.append(Line(bold("DESCRIPTION")))
      lines.append(Line(renderDescription(indent(description, indentAmount))))
      lines.append(Line.Empty)
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
    lines.append(Line(indentSpace.style + paramName + shortFlagDescription + qualifierString.style + paramHelp.summaryOpt.fold("")(" - " + _).style))
    for (description ← paramHelp.descriptionOpt)
      lines.append(Line(indent(description, indentAmount * 2).style))
    lines.append(Line.Empty)
    lines
  }

  def renderFieldHelp(obj: MashObject): Seq[Line] = {
    val lines = ArrayBuffer[Line]()
    val fieldHelp = FieldHelpClass.Wrapper(obj)
    lines.append(Line(bold("FIELD")))
    lines.append(Line(indentSpace.style + bold(fieldHelp.name) + fieldHelp.summaryOpt.fold("")(" - " + _).style))
    lines.append(Line.Empty)
    lines.append(Line(bold("CLASS")))
    lines.append(Line((indentSpace + fieldHelp.klass).style))
    lines.append(Line.Empty)
    for (description ← fieldHelp.descriptionOpt) {
      lines.append(Line(bold("DESCRIPTION")))
      lines.append(Line(indent(description, indentAmount).style))
      lines.append(Line.Empty)
    }
    lines
  }

  private def renderMethodSummary(methodHelp: FunctionHelpClass.Wrapper): Line =
    Line(indentSpace.style + fieldMethodStyle(methodHelp.name) + methodHelp.summaryOpt.fold("")(" - " + _).style)

  def renderClassHelp(obj: MashObject): Seq[Line] = {
    val lines = ArrayBuffer[Line]()

    val classHelp = ClassHelpClass.Wrapper(obj)
    lines.append(Line(bold("CLASS")))
    val summaryOpt = classHelp.summaryOpt
    lines += Line(indentSpace.style + bold(classHelp.fullyQualifiedName) + summaryOpt.fold("")(" - " + _).style)
    for (description ← classHelp.descriptionOpt.map(_.asInstanceOf[MashString])) {
      lines.append(Line.Empty)
      lines.append(Line(bold("DESCRIPTION")))
      lines += Line(indent(description.s, indentAmount).style)
    }
    for (parent ← classHelp.parentOpt) {
      lines.append(Line.Empty)
      lines.append(Line(bold("PARENT")))
      lines += Line((indentSpace + parent).style)
    }
    val fields = classHelp.fields
    if (fields.nonEmpty) {
      lines.append(Line.Empty)
      lines.append(Line(bold("FIELDS")))
      for (field ← fields)
        lines.append(Line(indentSpace.style + fieldMethodStyle(field.name) + field.summaryOpt.fold("")(" - " + _).style))
    }
    val staticMethods = classHelp.staticMethods
    if (staticMethods.nonEmpty) {
      lines.append(Line.Empty)
      lines.append(Line(bold("STATIC METHODS")))
      lines.appendAll(staticMethods.map(renderMethodSummary))
    }
    val methods = classHelp.methods
    if (methods.nonEmpty) {
      lines.append(Line.Empty)
      lines.append(Line(bold("METHODS")))
      lines.appendAll(methods.map(renderMethodSummary))
    }

    lines
  }

  private val MashMarkupPattern = Pattern.compile("<mash>(.+?)</mash>", Pattern.DOTALL)

  private def renderDescription(s: String): StyledString = {
    val matcher = MashMarkupPattern.matcher(s)
    val chunks = ArrayBuffer[StyledString]()
    var previous = 0
    while (matcher.find()) {
      val region = Region(previous, matcher.start)
      chunks += region.of(s).style
      previous = matcher.end
      chunks += new MashRenderer().renderChars(matcher.group(1))
    }
    chunks += s.substring(previous).style
    StyledString.empty.join(chunks)
  }

}
