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

object Printer {

  def replaceProblematicChars(s: String): String = s.map {
    case '\t' | '\n' | '\r' | '\b' ⇒ ' '
    case c                         ⇒ c
  }

}

class Printer(output: PrintStream, terminalInfo: TerminalInfo) {

  private val helpPrinter = new HelpPrinter(output)

  def render(x: Any) = x match {
    case xs: Seq[_] if xs.nonEmpty && xs.forall(x ⇒ x == null || x.isInstanceOf[MashString]) ⇒
      xs.foreach(output.println)
    case xs: Seq[_] if xs.nonEmpty && xs.forall(_.isInstanceOf[MashObject]) ⇒
      new ObjectTablePrinter(output, terminalInfo).printTable(xs.asInstanceOf[Seq[MashObject]])
    case mo: MashObject if mo.classOpt == Some(FunctionHelpClass) ⇒
      helpPrinter.printFunctionHelp(mo)
    case mo: MashObject if mo.classOpt == Some(FieldHelpClass) ⇒
      helpPrinter.printFieldHelp(mo)
    case mo: MashObject if mo.classOpt == Some(ClassHelpClass) ⇒
      helpPrinter.printClassHelp(mo)
    case mo: MashObject ⇒ new ObjectPrinter(output, terminalInfo).printObject(mo)
    case xs: Seq[_] if xs.nonEmpty && xs.forall(_ == ((): Unit)) ⇒ // Don't print out sequence of unit
    case () ⇒ // Don't print out Unit 
    case _ ⇒
      val f = StringUtils.ellipsisise(renderField(x), maxLength = terminalInfo.columns)
      output.println(f)
  }

  def renderField(x: Any, inCell: Boolean = false): String = x match {
    case mo: MashObject if mo.classOpt == Some(PermissionsClass) ⇒ PermissionsPrinter.permissionsString(mo)
    case mo: MashObject if mo.classOpt == Some(PermissionsSectionClass) ⇒ PermissionsPrinter.permissionsSectionString(mo)
    case MashNumber(n, Some(BytesClass)) ⇒ BytesPrinter.humanReadable(n)
    case MashNumber(n, _) ⇒ NumberUtils.prettyString(n)
    case i: Instant ⇒ new PrettyTime().format(Date.from(i))
    case xs: Seq[Any] if inCell ⇒ xs.map(renderField(_)).mkString(", ")
    case xs: Seq[Any] ⇒ xs.map(renderField(_)).mkString("[", ", ", "]")
    case _ ⇒ Printer.replaceProblematicChars(ToStringifier.stringify(x))
  }

}