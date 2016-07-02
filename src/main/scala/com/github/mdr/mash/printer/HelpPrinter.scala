package com.github.mdr.mash.printer

import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.ns.core.help.ParameterHelpClass
import com.github.mdr.mash.ns.core.help.FunctionHelpClass
import com.github.mdr.mash.runtime.MashObject
import org.fusesource.jansi.Ansi
import org.fusesource.jansi.Ansi.Color
import java.util.regex.Pattern
import com.github.mdr.mash.ns.core.help.FieldHelpClass
import com.github.mdr.mash.ns.core.help.ClassHelpClass
import java.io.PrintStream
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashBoolean

/**
 * Render function/method/field help objects in a similar style to man pages
 */
class HelpPrinter(output: PrintStream) {

  private val indent = 8

  private val indentSpace = " " * indent

  def bold(s: Any) = Ansi.ansi().bold().a("" + s).boldOff().toString

  def printFunctionHelp(mo: MashObject) {
    import FunctionHelpClass.Fields._
    output.println(bold("NAME"))
    output.println(indentSpace + bold(mo(FullyQualifiedName)) + " - " + mo(Summary))
    output.println()
    val classOpt = MashNull.option(mo(Class)).map(_.asInstanceOf[MashString].s)
    for (klass ← classOpt) {
      output.println(bold("CLASS"))
      output.println(indentSpace + klass)
      output.println()
    }
    output.println(bold("CALLING SYNTAX"))
    val maybeTarget = if (classOpt.isDefined) "target." else ""
    output.println(indentSpace + maybeTarget + mo(CallingSyntax))
    output.println()
    val parameters = mo(Parameters).asInstanceOf[MashList].items
    if (parameters.nonEmpty) {
      import ParameterHelpClass.Fields._
      output.println(bold("PARAMETERS"))
      for (param ← parameters)
        printParameterHelp(param.asInstanceOf[MashObject])
    }
    for (description ← MashNull.option(mo(Description)).map(_.asInstanceOf[MashString])) {
      output.println(bold("DESCRIPTION"))
      output.println(shiftLeftMargin(description.s, indent))
      output.println()
    }
  }

  def printFieldHelp(obj: MashObject) {
    import FieldHelpClass.Fields._
    val fieldHelp = FieldHelpClass.Wrapper(obj)
    output.println(bold("FIELD"))
    output.println(indentSpace + bold(fieldHelp.name) + " - " + fieldHelp.summary)
    output.println()
    output.println(bold("CLASS"))
    output.println(indentSpace + fieldHelp.klass)
    output.println()
    for (description ← fieldHelp.descriptionOpt) {
      output.println(bold("DESCRIPTION"))
      output.println(shiftLeftMargin(description, indent))
      output.println()
    }
  }

  def printParameterHelp(param: MashObject) {
    import ParameterHelpClass.Fields._
    def paramNameStyle(s: Any) = Ansi.ansi().bold().fg(Color.BLUE).a("" + s).boldOff().fg(Color.DEFAULT).toString
    output.print(indentSpace)
    def boolParam(field: Field) = param(field).asInstanceOf[MashBoolean].value
    var qualifiers: Seq[String] = Seq()
    val isFlag = boolParam(IsFlagParameter)
    if (boolParam(IsLast))
      qualifiers +:= "last"
    if (boolParam(IsOptional))
      qualifiers +:= "optional"
    if (boolParam(IsVariadic))
      qualifiers +:= "variadic"
    val qualifierString = qualifiers match {
      case Seq() ⇒ ""
      case _     ⇒ qualifiers.mkString(" [", ", ", "]")
    }
    val paramName = paramNameStyle("" + (if (isFlag) "--" + param(Name) else param(Name)))
    val shortFlagDescription = paramNameStyle(MashNull.option(param(ShortFlag)).map(f ⇒ s" | -$f").getOrElse(""))
    output.println(paramName + shortFlagDescription + qualifierString + " - " + param(Summary))
    for (description ← MashNull.option(param(Description)).map(_.asInstanceOf[MashString]))
      output.println(shiftLeftMargin(description.s, indent * 2))
    output.println()
  }

  def printClassHelp(mo: MashObject) {
    def fieldMethodStyle(s: Any) = Ansi.ansi().bold().fg(Color.BLUE).a("" + s).boldOff().fg(Color.DEFAULT).toString
    import ClassHelpClass.Fields._
    output.println(bold("NAME"))
    output.println(indentSpace + bold(mo(FullyQualifiedName)) + " - " + mo(Summary))
    output.println()
    for (description ← MashNull.option(mo(Description)).map(_.asInstanceOf[MashString])) {
      output.println(bold("DESCRIPTION"))
      output.println(shiftLeftMargin(description.s, indent))
      output.println()
    }
    for (parent ← MashNull.option(mo(Parent)).map(_.asInstanceOf[MashString])) {
      output.println(bold("PARENT"))
      output.println(indentSpace + mo(Parent))
      output.println()
    }
    val fields = mo(Fields).asInstanceOf[MashList]
    if (fields.nonEmpty) {
      output.println(bold("FIELDS"))
      for (field ← fields) {
        val fieldObject = field.asInstanceOf[MashObject]
        import FieldHelpClass.Fields._
        output.print(indentSpace)
        output.print(fieldMethodStyle(fieldObject(FieldHelpClass.Fields.Name)))
        output.print(" - " + fieldObject(FieldHelpClass.Fields.Summary))
        output.println()
      }
    }
    val methods = mo(Methods).asInstanceOf[MashList].items
    if (methods.nonEmpty) {
      output.println(bold("METHODS"))
      for (method ← methods) {
        val methodObject = method.asInstanceOf[MashObject]
        import FieldHelpClass.Fields._
        output.print(indentSpace)
        output.print(fieldMethodStyle(methodObject(FieldHelpClass.Fields.Name)))
        output.print(" - " + methodObject(FieldHelpClass.Fields.Summary))
        output.println()
      }
    }

  }

  private def shiftLeftMargin(s: String, indent: Int) =
    Pattern.compile("^", Pattern.MULTILINE).matcher(s).replaceAll(" " * indent)

}