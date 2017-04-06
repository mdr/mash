package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.classes.Field
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.ns.core.help.{ ClassHelpClass, FieldHelpClass, FunctionHelpClass, ParameterHelpClass }
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.screen.BasicColour
import com.github.mdr.mash.screen.Screen._
import com.github.mdr.mash.utils.StringUtils.indent
import com.github.mdr.mash.screen.Style.StylableString

/**
  * Render function/method/field help objects in a similar style to man pages
  */
class HelpPrinter(output: PrintStream) {

  private val indentAmount = 8

  private val indentSpace = " " * indentAmount

  def printFunctionHelp(obj: MashObject) {
    import FunctionHelpClass.Fields._
    val help = FunctionHelpClass.Wrapper(obj)
    output.println(bold(if (help.classOpt.isDefined) "METHOD" else "FUNCTION"))
    val name = help.fullyQualifiedName
    val names = (name +: help.aliases).map(bold(_)).mkString(", ")
    output.println(indentSpace + bold(names) + help.summaryOpt.fold("")(" - " + _))
    output.println()
    for (klass ← help.classOpt) {
      output.println(bold("CLASS"))
      output.println(indentSpace + klass)
      output.println()
    }
    output.println(bold("CALLING SYNTAX"))
    val maybeTarget = if (help.classOpt.isDefined) "target." else ""
    output.println(indentSpace + maybeTarget + obj(CallingSyntax))
    output.println()
    val parameters = help.parameters
    if (parameters.nonEmpty) {
      output.println(bold("PARAMETERS"))
      for (param ← parameters)
        printParameterHelp(param.asInstanceOf[MashObject])
    }
    for (description ← help.descriptionOpt) {
      output.println(bold("DESCRIPTION"))
      output.println(indent(description, indentAmount))
      output.println()
    }
  }

  def printFieldHelp(obj: MashObject) {
    val fieldHelp = FieldHelpClass.Wrapper(obj)
    output.println(bold("FIELD"))
    output.println(indentSpace + bold(fieldHelp.name) + fieldHelp.summaryOpt.fold("")(" - " + _))
    output.println()
    output.println(bold("CLASS"))
    output.println(indentSpace + fieldHelp.klass)
    output.println()
    for (description ← fieldHelp.descriptionOpt) {
      output.println(bold("DESCRIPTION"))
      output.println(indent(description, indentAmount))
      output.println()
    }
  }

  def printParameterHelp(param: MashObject) {
    val paramHelp = ParameterHelpClass.Wrapper(param)
    output.print(indentSpace)
    var qualifiers: Seq[String] = Seq()
    val isFlag = paramHelp.isFlag
    if (paramHelp.isLast)
      qualifiers +:= "last"
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
    output.println(paramName + shortFlagDescription + qualifierString + paramHelp.summaryOpt.fold("")(" - " + _))
    for (description ← paramHelp.descriptionOpt)
      output.println(indent(description, indentAmount * 2))
    output.println()
  }

  def printClassHelp(obj: MashObject) {
    import ClassHelpClass.Fields._
    output.println(bold("CLASS"))
    val summaryOpt = MashNull.option(obj(Summary))
    output.println(indentSpace + bold(obj(FullyQualifiedName).toString) + summaryOpt.fold("")(" - " + _))
    for (description ← MashNull.option(obj(Description)).map(_.asInstanceOf[MashString])) {
      output.println()
      output.println(bold("DESCRIPTION"))
      output.println(indent(description.s, indentAmount))
    }
    for (parent ← MashNull.option(obj(Parent)).map(_.asInstanceOf[MashString])) {
      output.println()
      output.println(bold("PARENT"))
      output.println(indentSpace + obj(Parent))
    }
    val fields = obj(Fields).asInstanceOf[MashList]
    if (fields.nonEmpty) {
      output.println()
      output.println(bold("FIELDS"))
      for (field ← fields) {
        val fieldObject = field.asInstanceOf[MashObject]
        output.print(indentSpace)
        output.print(fieldMethodStyle(fieldObject(FieldHelpClass.Fields.Name).toString))
        for (summary ← MashNull.option(fieldObject(FieldHelpClass.Fields.Summary)))
          output.print(" - " + summary)
        output.println()
      }
    }
    val staticMethods = obj(StaticMethods).asInstanceOf[MashList].elements
    if (staticMethods.nonEmpty) {
      output.println()
      output.println(bold("STATIC METHODS"))
      staticMethods.foreach(printMethodSummary)
    }
    val methods = obj(Methods).asInstanceOf[MashList].elements
    if (methods.nonEmpty) {
      output.println()
      output.println(bold("METHODS"))
      methods.foreach(printMethodSummary)
    }

  }

  private def printMethodSummary(method: MashValue): Unit = {
    val methodObject = method.asInstanceOf[MashObject]
    output.print(indentSpace)
    output.print(fieldMethodStyle(methodObject(FieldHelpClass.Fields.Name).toString))
    for (summary ← MashNull.option(methodObject(FieldHelpClass.Fields.Summary)))
      output.print(" - " + summary)
    output.println()
  }

  private def paramNameStyle(s: String) = drawStyledChars(s.toString.style(foregroundColour = BasicColour.Blue, bold = true))

  private def fieldMethodStyle(s: String) = drawStyledChars(s.toString.style(foregroundColour = BasicColour.Blue, bold = true))

  private def bold(s: String) = drawStyledChars(s.toString.style(bold = true))

}