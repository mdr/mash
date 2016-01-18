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

/**
 * Render function/method/field help objects in a similar style to man pages
 */
object HelpPrinter {

  private val indent = 8

  private val indentSpace = " " * indent

  def bold(s: Any) = Ansi.ansi().bold().a("" + s).boldOff().toString

  def printFunctionHelp(mo: MashObject) {
    import FunctionHelpClass.Fields._
    println(bold("NAME"))
    println(indentSpace + bold(mo.field(FullyQualifiedName)) + " - " + mo.field(Summary))
    println()
    val classOpt = Option(mo.field(Class).asInstanceOf[MashString]).map(_.s)
    for (klass ← classOpt) {
      println(bold("CLASS"))
      println(indentSpace + klass)
      println()
    }
    println(bold("CALLING SYNTAX"))
    val maybeTarget = if (classOpt.isDefined) "target." else ""
    println(indentSpace + maybeTarget + mo.field(CallingSyntax))
    println()
    val parameters = mo.field(Parameters).asInstanceOf[Seq[MashObject]]
    if (parameters.nonEmpty) {
      import ParameterHelpClass.Fields._
      println(bold("PARAMETERS"))
      println()
      for (param ← parameters)
        printParameterHelp(param)
    }
    for (description ← Option(mo.field(Description).asInstanceOf[MashString])) {
      println(bold("DESCRIPTION"))
      println(shiftLeftMargin(description.s, indent))
      println()
    }
  }

  def printFieldHelp(mo: MashObject) {
    import FieldHelpClass.Fields._
    println(bold("FIELD"))
    println(indentSpace + bold(mo.field(Name)) + " - " + mo.field(Summary))
    println()
    println(bold("CLASS"))
    println(indentSpace + mo.field(Class))
    println()
    for (description ← Option(mo.field(Description).asInstanceOf[MashString])) {
      println(bold("DESCRIPTION"))
      println(shiftLeftMargin(description.s, 8))
      println()
    }
  }

  def printParameterHelp(param: MashObject) {
    import ParameterHelpClass.Fields._
    def paramNameStyle(s: Any) = Ansi.ansi().bold().fg(Color.BLUE).a("" + s).boldOff().fg(Color.DEFAULT).toString
    print(indentSpace)
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
    println(paramName + shortFlagDescription + qualifierString + " - " + param.field(Summary))
    for (description ← Option(param.field(Description).asInstanceOf[MashString]))
      println(shiftLeftMargin(description.s, indent * 2))
    println()
  }

  def printClassHelp(mo: MashObject) {
    def fieldMethodStyle(s: Any) = Ansi.ansi().bold().fg(Color.BLUE).a("" + s).boldOff().fg(Color.DEFAULT).toString
    import ClassHelpClass.Fields._
    println(bold("NAME"))
    println(indentSpace + bold(mo.field(FullyQualifiedName)) + " - " + mo.field(Summary))
    println()
    for (description ← Option(mo.field(Description).asInstanceOf[MashString])) {
      println(bold("DESCRIPTION"))
      println(shiftLeftMargin(description.s, indent))
      println()
    }
    for (parent ← Option(mo.field(Parent).asInstanceOf[MashString])) {
      println(bold("PARENT"))
      println(indentSpace + mo.field(Parent))
      println()
    }
    val fields = mo.field(Fields).asInstanceOf[Seq[MashObject]]
    if (fields.nonEmpty) {
      println(bold("FIELDS"))
      println()
      for (field ← fields) {
        import FieldHelpClass.Fields._
        print(indentSpace)
        print(fieldMethodStyle(field.field(FieldHelpClass.Fields.Name)))
        print(" - " + field.field(FieldHelpClass.Fields.Summary))
        println()
      }
    }
    val methods = mo.field(Methods).asInstanceOf[Seq[MashObject]]
    if (methods.nonEmpty) {
      println(bold("METHODS"))
      println()
      for (method ← methods) {
        import FieldHelpClass.Fields._
        print(indentSpace)
        print(fieldMethodStyle(method.field(FieldHelpClass.Fields.Name)))
        print(" - " + method.field(FieldHelpClass.Fields.Summary))
        println()
      }
    }

  }

  private def shiftLeftMargin(s: String, indent: Int) =
    Pattern.compile("^", Pattern.MULTILINE).matcher(s).replaceAll(" " * indent)

}