package com.github.mdr.mash.printer

import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.ns.core.help.ParameterHelpClass
import com.github.mdr.mash.ns.core.help.FunctionHelpClass
import com.github.mdr.mash.evaluator.MashObject
import org.fusesource.jansi.Ansi
import org.fusesource.jansi.Ansi.Color
import java.util.regex.Pattern
import com.github.mdr.mash.ns.core.help.FieldHelpClass
import com.github.mdr.mash.ns.core.help.ClassHelpClass
import java.io.PrintStream
import com.github.mdr.mash.evaluator.MashList

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
    output.println(indentSpace + bold(mo.field(FullyQualifiedName)) + " - " + mo.field(Summary))
    output.println()
    val classOpt = Option(mo.field(Class).asInstanceOf[MashString]).map(_.s)
    for (klass ← classOpt) {
      output.println(bold("CLASS"))
      output.println(indentSpace + klass)
      output.println()
    }
    output.println(bold("CALLING SYNTAX"))
    val maybeTarget = if (classOpt.isDefined) "target." else ""
    output.println(indentSpace + maybeTarget + mo.field(CallingSyntax))
    output.println()
    val parameters = mo.field(Parameters).asInstanceOf[MashList].items
    if (parameters.nonEmpty) {
      import ParameterHelpClass.Fields._
      output.println(bold("PARAMETERS"))
      for (param ← parameters)
        printParameterHelp(param.asInstanceOf[MashObject])
    }
    for (description ← Option(mo.field(Description).asInstanceOf[MashString])) {
      output.println(bold("DESCRIPTION"))
      output.println(shiftLeftMargin(description.s, indent))
      output.println()
    }
  }

  def printFieldHelp(mo: MashObject) {
    import FieldHelpClass.Fields._
    output.println(bold("FIELD"))
    output.println(indentSpace + bold(mo.field(Name)) + " - " + mo.field(Summary))
    output.println()
    output.println(bold("CLASS"))
    output.println(indentSpace + mo.field(Class))
    output.println()
    for (description ← Option(mo.field(Description).asInstanceOf[MashString])) {
      output.println(bold("DESCRIPTION"))
      output.println(shiftLeftMargin(description.s, 8))
      output.println()
    }
  }

  def printParameterHelp(param: MashObject) {
    import ParameterHelpClass.Fields._
    def paramNameStyle(s: Any) = Ansi.ansi().bold().fg(Color.BLUE).a("" + s).boldOff().fg(Color.DEFAULT).toString
    output.print(indentSpace)
    var qualifiers: Seq[String] = Seq()
    val isFlag = param.field(IsFlagParameter).asInstanceOf[Boolean]
    if (param.field(IsLast).asInstanceOf[Boolean])
      qualifiers +:= "last"
    if (param.field(IsOptional).asInstanceOf[Boolean])
      qualifiers +:= "optional"
    if (param.field(IsVariadic).asInstanceOf[Boolean])
      qualifiers +:= "variadic"
    val qualifierString = qualifiers match {
      case Seq() ⇒ ""
      case _     ⇒ qualifiers.mkString(" [", ", ", "]")
    }
    val paramName = paramNameStyle("" + (if (isFlag) "--" + param.field(Name) else param.field(Name)))
    val shortFlagDescription = paramNameStyle(Option(param.field(ShortFlag).asInstanceOf[MashString]).map(f ⇒ s" | -$f").getOrElse(""))
    output.println(paramName + shortFlagDescription + qualifierString + " - " + param.field(Summary))
    for (description ← Option(param.field(Description).asInstanceOf[MashString]))
      output.println(shiftLeftMargin(description.s, indent * 2))
    output.println()
  }

  def printClassHelp(mo: MashObject) {
    def fieldMethodStyle(s: Any) = Ansi.ansi().bold().fg(Color.BLUE).a("" + s).boldOff().fg(Color.DEFAULT).toString
    import ClassHelpClass.Fields._
    output.println(bold("NAME"))
    output.println(indentSpace + bold(mo.field(FullyQualifiedName)) + " - " + mo.field(Summary))
    output.println()
    for (description ← Option(mo.field(Description).asInstanceOf[MashString])) {
      output.println(bold("DESCRIPTION"))
      output.println(shiftLeftMargin(description.s, indent))
      output.println()
    }
    for (parent ← Option(mo.field(Parent).asInstanceOf[MashString])) {
      output.println(bold("PARENT"))
      output.println(indentSpace + mo.field(Parent))
      output.println()
    }
    val fields = mo.field(Fields).asInstanceOf[MashList]
    if (fields.nonEmpty) {
      output.println(bold("FIELDS"))
      for (field ← fields) {
        val fieldObject = field.asInstanceOf[MashObject]
        import FieldHelpClass.Fields._
        output.print(indentSpace)
        output.print(fieldMethodStyle(fieldObject.field(FieldHelpClass.Fields.Name)))
        output.print(" - " + fieldObject.field(FieldHelpClass.Fields.Summary))
        output.println()
      }
    }
    val methods = mo.field(Methods).asInstanceOf[MashList]
    if (methods.nonEmpty) {
      output.println(bold("METHODS"))
      for (method ← methods) {
        val methodObject = method.asInstanceOf[MashObject]
        import FieldHelpClass.Fields._
        output.print(indentSpace)
        output.print(fieldMethodStyle(methodObject.field(FieldHelpClass.Fields.Name)))
        output.print(" - " + methodObject.field(FieldHelpClass.Fields.Summary))
        output.println()
      }
    }

  }

  private def shiftLeftMargin(s: String, indent: Int) =
    Pattern.compile("^", Pattern.MULTILINE).matcher(s).replaceAll(" " * indent)

}