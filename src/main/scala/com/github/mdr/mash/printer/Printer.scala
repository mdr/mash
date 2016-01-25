package com.github.mdr.mash.printer

import java.io.PrintStream
import java.time.Instant
import java.util.Date
import org.ocpsoft.prettytime.PrettyTime
import com.github.mdr.mash.evaluator.MashNumber
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.ns.core.BytesClass
import com.github.mdr.mash.ns.core.help.ClassHelpClass
import com.github.mdr.mash.ns.core.help.FieldHelpClass
import com.github.mdr.mash.ns.core.help.FunctionHelpClass
import com.github.mdr.mash.ns.os.PermissionsClass
import com.github.mdr.mash.ns.os.PermissionsSectionClass
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.NumberUtils
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.evaluator.BoundMethod
import com.github.mdr.mash.evaluator.MashList

object Printer {

  def replaceProblematicChars(s: String): String = s.map {
    case '\t' | '\n' | '\r' | '\b' ⇒ ' '
    case c                         ⇒ c
  }

}

class Printer(output: PrintStream, terminalInfo: TerminalInfo) {

  private val helpPrinter = new HelpPrinter(output)

  def render(x: Any) = x match {
    case xs: MashList if xs.nonEmpty && xs.forall(x ⇒ x == null || x.isInstanceOf[MashString]) ⇒
      xs.foreach(output.println)
    case xs: MashList if xs.nonEmpty && xs.forall(_.isInstanceOf[MashObject]) ⇒
      new ObjectTablePrinter(output, terminalInfo).printTable(xs.items.asInstanceOf[Seq[MashObject]])
    case mo: MashObject if mo.classOpt == Some(FunctionHelpClass) ⇒
      helpPrinter.printFunctionHelp(mo)
    case mo: MashObject if mo.classOpt == Some(FieldHelpClass) ⇒
      helpPrinter.printFieldHelp(mo)
    case mo: MashObject if mo.classOpt == Some(ClassHelpClass) ⇒
      helpPrinter.printClassHelp(mo)
    case mo: MashObject ⇒ new ObjectPrinter(output, terminalInfo).printObject(mo)
    case xs: MashList if xs.nonEmpty && xs.forall(_ == ((): Unit)) ⇒ // Don't print out sequence of unit
    case f: MashFunction ⇒
      renderFunction(f)
    case bm: BoundMethod ⇒
      renderBoundMethod(bm)
    case () ⇒ // Don't print out Unit 
    case _ ⇒
      val f = StringUtils.ellipsisise(renderField(x), maxLength = terminalInfo.columns)
      output.println(f)
  }

  def renderBox(title: String, lines: Seq[String]) {
    val boxWidth = math.min(math.max(lines.map(_.size + 4).max, title.size + 4), terminalInfo.columns)
    val innerWidth = boxWidth - 4
    val displayTitle = " " + StringUtils.ellipsisise(title, innerWidth) + " "
    val displayLines = lines.map(l ⇒ StringUtils.ellipsisise(l, innerWidth))
    val topLine = "┌─" + displayTitle + "─" * (innerWidth - displayTitle.length) + "─┐"
    val bottomLine = "└─" + "─" * innerWidth + "─┘"
    val contentLines = displayLines.map(l ⇒ "│ " + l + " " * (innerWidth - l.length) + " │")
    for (line ← (topLine +: contentLines :+ bottomLine))
      output.println(line)
  }

  private def renderFunction(f: MashFunction) {
    val title = f.name
    val lines = Seq(f.summary, "", f.name + " " + f.params.callingSyntax)
    renderBox(title, lines)
  }

  private def renderBoundMethod(boundMethod: BoundMethod) {
    val title = boundMethod.name
    val method = boundMethod.method
    val lines = Seq(method.summary, "", "target." + method.name + " " + method.params.callingSyntax)
    renderBox(title, lines)
  }

  def renderField(x: Any, inCell: Boolean = false): String = x match {
    case mo: MashObject if mo.classOpt == Some(PermissionsClass) ⇒ PermissionsPrinter.permissionsString(mo)
    case mo: MashObject if mo.classOpt == Some(PermissionsSectionClass) ⇒ PermissionsPrinter.permissionsSectionString(mo)
    case MashNumber(n, Some(BytesClass)) ⇒ BytesPrinter.humanReadable(n)
    case MashNumber(n, _) ⇒ NumberUtils.prettyString(n)
    case i: Instant ⇒ new PrettyTime().format(Date.from(i))
    case xs: MashList if inCell ⇒ xs.items.map(renderField(_)).mkString(", ")
    case xs: MashList ⇒ xs.items.map(renderField(_)).mkString("[", ", ", "]")
    case _ ⇒ Printer.replaceProblematicChars(ToStringifier.stringify(x))
  }

}