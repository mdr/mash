package com.github.mdr.mash.printer

import java.io.PrintStream
import java.time.{ Instant, ZoneId, ZonedDateTime }
import java.time.format.{ DateTimeFormatter, FormatStyle }
import java.util.Date

import com.github.mdr.mash.evaluator.{ BoundMethod, MashClass, ToStringifier }
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.core.BytesClass
import com.github.mdr.mash.ns.core.help.{ ClassHelpClass, FieldHelpClass, FunctionHelpClass, HelpFunction }
import com.github.mdr.mash.ns.git.StatusClass
import com.github.mdr.mash.ns.os.{ PermissionsClass, PermissionsSectionClass }
import com.github.mdr.mash.ns.time.{ MillisecondsClass, SecondsClass }
import com.github.mdr.mash.ns.view.ViewClass
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.{ NumberUtils, StringUtils }
import org.ocpsoft.prettytime.PrettyTime

case class ViewConfig(fuzzyTime: Boolean)

object Printer {

  val prettyTime = new PrettyTime

  def replaceProblematicChars(s: String): String = s.map {
    case '\t' | '\n' | '\r' | '\b' ⇒ ' '
    case c                         ⇒ c
  }

}

class FieldRenderer(viewConfig: ViewConfig) {

  private val dateTimeFormatter = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM)

  def renderField(value: MashValue, inCell: Boolean = false): String = value match {
    case MashBoolean.True if inCell                         ⇒ "✓"
    case MashBoolean.False if inCell                        ⇒ "✗"
    case obj @ MashObject(_, Some(PermissionsClass))        ⇒ PermissionsPrinter.permissionsString(obj)
    case obj @ MashObject(_, Some(PermissionsSectionClass)) ⇒ PermissionsPrinter.permissionsSectionString(obj)
    case MashNumber(n, Some(BytesClass))                    ⇒ BytesPrinter.humanReadable(n)
    case MashNumber(n, Some(MillisecondsClass))             ⇒ NumberUtils.prettyString(n) + "ms"
    case MashNumber(n, Some(SecondsClass))                  ⇒ NumberUtils.prettyString(n) + "s"
    case MashNumber(n, _)                                   ⇒ NumberUtils.prettyString(n)
    case MashWrapped(i: Instant) if viewConfig.fuzzyTime    ⇒ Printer.prettyTime.format(Date.from(i))
    case MashWrapped(i: Instant)                            ⇒ dateTimeFormatter.format(ZonedDateTime.ofInstant(i, ZoneId.systemDefault))
    case xs: MashList if inCell                             ⇒ xs.items.map(renderField(_)).mkString(", ")
    case xs: MashList                                       ⇒ xs.items.map(renderField(_)).mkString("[", ", ", "]")
    case _                                                  ⇒
      val s = ToStringifier.safeStringify(value)
      if (inCell) Printer.replaceProblematicChars(s) else s
  }

}

case class PrintResult(printModelOpt: Option[PrintModel] = None)

class Printer(output: PrintStream, terminalInfo: TerminalInfo, viewConfig: ViewConfig = ViewConfig(fuzzyTime = true)) {

  private val helpPrinter = new HelpPrinter(output)
  private val fieldRenderer = new FieldRenderer(viewConfig)

  private def getPrintModel(value: MashValue): PrintModel = value match {
    case obj: MashObject                                       =>
      new ObjectModelCreator(terminalInfo, viewConfig).create(obj)
    case xs: MashList if xs.forall(_.isInstanceOf[MashObject]) =>
      val objects = xs.items.asInstanceOf[Seq[MashObject]]
      new ObjectsTableModelCreator(terminalInfo, showSelections = true, viewConfig).create(objects, xs)
    case _                                                     =>
      new ValueModelCreator(terminalInfo, viewConfig).create(value)
  }

  def print(value: MashValue,
            disableCustomViews: Boolean = false,
            alwaysUseBrowser: Boolean = false,
            alwaysUseTreeBrowser: Boolean = false): PrintResult =
    if (alwaysUseBrowser) {
      val model = getPrintModel(value)
      PrintResult(Some(model))
    } else {
      value match {
        case _: MashList | _: MashObject if alwaysUseTreeBrowser                                       =>
          val model = new ObjectTreeModelCreator(viewConfig).create(value)
          return PrintResult(Some(model))
        case xs: MashList if xs.nonEmpty && xs.forall(_.isInstanceOf[MashObject])                      ⇒
          val objects = xs.items.asInstanceOf[Seq[MashObject]]
          val nonDataRows = 4 // 3 header rows + 1 footer
          if (objects.size > terminalInfo.rows - nonDataRows) {
            val model = new ObjectsTableModelCreator(terminalInfo, showSelections = true, viewConfig).create(objects, xs)
            return PrintResult(Some(model))
          } else
            new ObjectsTablePrinter(output, terminalInfo, viewConfig).printTable(objects)
        case xs: MashList if xs.nonEmpty && xs.forall(x ⇒ x == MashNull || x.isInstanceOf[MashString]) ⇒
          xs.items.foreach(output.println)
        case obj: MashObject if obj.classOpt == Some(ViewClass)                                        ⇒
          val data = obj(ViewClass.Fields.Data)
          val disableCustomViews = obj(ViewClass.Fields.DisableCustomViews) == MashBoolean.True
          val alwaysUseBrowser = obj(ViewClass.Fields.UseBrowser) == MashBoolean.True
          val alwaysUseTreeBrowser = obj(ViewClass.Fields.UseTree) == MashBoolean.True
          return print(data, disableCustomViews = disableCustomViews, alwaysUseBrowser = alwaysUseBrowser, alwaysUseTreeBrowser = alwaysUseTreeBrowser)
        case obj: MashObject if obj.classOpt == Some(FunctionHelpClass) && !disableCustomViews         ⇒
          helpPrinter.printFunctionHelp(obj)
        case obj: MashObject if obj.classOpt == Some(FieldHelpClass) && !disableCustomViews            ⇒
          helpPrinter.printFieldHelp(obj)
        case obj: MashObject if obj.classOpt == Some(ClassHelpClass) && !disableCustomViews            ⇒
          helpPrinter.printClassHelp(obj)
        case obj: MashObject if obj.classOpt == Some(StatusClass) && !disableCustomViews               ⇒
          new GitStatusPrinter(output).print(obj)
        case obj: MashObject                                                                           ⇒
          new ObjectPrinter(output, terminalInfo, viewConfig).printObject(obj)
        case xs: MashList if xs.nonEmpty && xs.forall(_ == ((): Unit))                                 ⇒ // Don't print out sequence of unit
        case f: MashFunction if !disableCustomViews                                                    ⇒
          print(HelpFunction.getHelp(f), disableCustomViews = disableCustomViews, alwaysUseBrowser = alwaysUseBrowser)
        case method: BoundMethod if !disableCustomViews                                                ⇒
          print(HelpFunction.getHelp(method), disableCustomViews = disableCustomViews, alwaysUseBrowser = alwaysUseBrowser)
        case klass: MashClass if !disableCustomViews                                                   ⇒
          print(HelpFunction.getHelp(klass), disableCustomViews = disableCustomViews, alwaysUseBrowser = alwaysUseBrowser)
        case MashUnit                                                                                  ⇒ // Don't print out Unit
        case _                                                                                         ⇒
          output.println(fieldRenderer.renderField(value, inCell = false))
      }
      PrintResult()
  }

  def printBox(title: String, lines: Seq[String]) {
    val boxWidth = math.min(math.max(lines.map(_.size + 4).max, title.size + 4), terminalInfo.columns)
    val innerWidth = boxWidth - 4
    val displayTitle = " " + StringUtils.ellipsisise(title, innerWidth) + " "
    val displayLines = lines.map(l ⇒ StringUtils.ellipsisise(l, innerWidth))
    val topLine = "┌─" + displayTitle + "─" * (innerWidth - displayTitle.length) + "─┐"
    val bottomLine = "└─" + "─" * innerWidth + "─┘"
    val contentLines = displayLines.map(l ⇒ "│ " + l + " " * (innerWidth - l.length) + " │")
    for (line ← topLine +: contentLines :+ bottomLine)
      output.println(line)
  }

}