package com.github.mdr.mash.printer

import java.time.Instant
import java.util.Date
import org.ocpsoft.prettytime.PrettyTime
import com.github.mdr.mash.evaluator.MashNumber
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.evaluator.MemberEvaluator
import com.github.mdr.mash.ns.core.BytesClass
import com.github.mdr.mash.ns.os.PermissionsClass
import com.github.mdr.mash.ns.os.PermissionsSectionClass
import com.github.mdr.mash.utils.NumberUtils
import com.github.mdr.mash.utils.StringUtils
import com.ibm.icu.text.DecimalFormat
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.ns.core.help.FunctionHelpClass
import org.fusesource.jansi.Ansi
import com.github.mdr.mash.ns.core.help.ParameterHelpClass
import java.util.regex.Pattern
import org.fusesource.jansi.Ansi.Color
import com.github.mdr.mash.ns.core.help.FieldHelpClass
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.ns.core.help.ClassHelpClass
import com.github.mdr.mash.terminal.TerminalInfo

object Printer {

  def replaceProblematicChars(s: String): String = s.map {
    case '\t' | '\n' | '\r' | '\b' ⇒ ' '
    case c                         ⇒ c
  }

}

class Printer(terminalInfo: TerminalInfo) {

  def render(x: Any) = x match {
    case xs: Seq[_] if xs.nonEmpty && xs.forall(x ⇒ x == null || x.isInstanceOf[MashString]) ⇒
      xs.foreach(println)
    case xs: Seq[_] if xs.nonEmpty && xs.forall(_.isInstanceOf[MashObject]) ⇒
      new ObjectTablePrinter(terminalInfo).printTable(xs.asInstanceOf[Seq[MashObject]])
    case mo: MashObject if mo.classOpt == Some(FunctionHelpClass) ⇒
      HelpPrinter.printFunctionHelp(mo)
    case mo: MashObject if mo.classOpt == Some(FieldHelpClass) ⇒
      HelpPrinter.printFieldHelp(mo)
    case mo: MashObject if mo.classOpt == Some(ClassHelpClass) ⇒
      HelpPrinter.printClassHelp(mo)
    case mo: MashObject ⇒ new ObjectPrinter(terminalInfo).printObject(mo)
    case xs: Seq[_] if xs.nonEmpty && xs.forall(_ == ((): Unit)) ⇒ // Don't print out sequence of unit
    case () ⇒ // Don't print out Unit 
    case _ ⇒
      val f = StringUtils.ellipsisise(renderField(x), maxLength = terminalInfo.columns)
      println(f)
  }

  private def shortRenderField(x: Any, inCell: Boolean = false) =
    StringUtils.ellipsisise(renderField(x, inCell = inCell), maxLength = 64)

  def renderField(x: Any, inCell: Boolean = false): String = x match {
    case mo: MashObject if mo.classOpt == Some(PermissionsClass) ⇒ permissionsString(mo)
    case mo: MashObject if mo.classOpt == Some(PermissionsSectionClass) ⇒ permissionsSectionString(mo)
    case MashNumber(n, Some(BytesClass)) ⇒ humanReadableBytes(n)
    case MashNumber(n, _) ⇒ NumberUtils.prettyString(n)
    case i: Instant ⇒ new PrettyTime().format(Date.from(i))
    case xs: Seq[Any] if inCell ⇒ xs.map(renderField(_)).mkString(", ")
    case xs: Seq[Any] ⇒ xs.map(renderField(_)).mkString("[", ", ", "]")
    case _ ⇒ Printer.replaceProblematicChars(ToStringifier.stringify(x))
  }

  private def humanReadableBytes(n: Double) = {
    val f = new DecimalFormat
    f.setMaximumSignificantDigits(3)
    if (n < 1024) {
      f.format(n) + "B"
    } else if (n < 1024 * 1024) {
      val kb = n / 1024
      f.format(kb) + "KB"
    } else if (n < 1024 * 1024 * 1024) {
      val mb = n / (1024 * 1024)
      f.format(mb) + "MB"
    } else {
      val gb = n / (1024 * 1024 * 1024)
      f.format(gb) + "GB"
    }
  }

  private def permissionsSectionString(section: MashObject): String = {
    val s = new StringBuilder
    def test(field: Field) = MemberEvaluator.lookup(section, field).asInstanceOf[Boolean]
    import PermissionsSectionClass.Fields
    if (test(Fields.CanRead)) s.append("r") else s.append("-")
    if (test(Fields.CanWrite)) s.append("w") else s.append("-")
    if (test(Fields.CanExecute)) s.append("x") else s.append("-")
    s.toString
  }

  private def permissionsString(perms: MashObject): String = {
    import PermissionsClass.Fields
    val s = new StringBuilder
    val owner = MemberEvaluator.lookup(perms, Fields.Owner).asInstanceOf[MashObject]
    val group = MemberEvaluator.lookup(perms, Fields.Group).asInstanceOf[MashObject]
    val others = MemberEvaluator.lookup(perms, Fields.Others).asInstanceOf[MashObject]
    s.append(permissionsSectionString(owner))
    s.append(permissionsSectionString(group))
    s.append(permissionsSectionString(others))
    s.toString
  }
}