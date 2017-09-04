package com.github.mdr.mash.printer

import java.io.PrintStream
import java.time.format.{ DateTimeFormatter, FormatStyle }
import java.time.{ Instant, ZoneId, ZonedDateTime }
import java.util.Date

import com.github.mdr.mash.classes.{ BoundMethod, MashClass }
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.core.BytesClass
import com.github.mdr.mash.ns.core.help._
import com.github.mdr.mash.ns.git.StatusClass
import com.github.mdr.mash.ns.os.{ PermissionsClass, PermissionsSectionClass }
import com.github.mdr.mash.ns.time.{ MillisecondsClass, SecondsClass }
import com.github.mdr.mash.ns.view.ViewClass
import com.github.mdr.mash.printer.model.TwoDTableModelCreator.isSuitableForTwoDTable
import com.github.mdr.mash.printer.model._
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.screen.StyledStringDrawer
import com.github.mdr.mash.utils.{ Dimensions, NumberUtils }
import org.ocpsoft.prettytime.PrettyTime

case class ViewConfig(fuzzyTime: Boolean = true,
                      browseLargeOutput: Boolean = true)

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
    case MashBoolean.True if inCell                       ⇒ "✓"
    case MashBoolean.False if inCell                      ⇒ "✗"
    case obj@MashObject(_, Some(PermissionsClass))        ⇒ PermissionsPrinter.permissionsString(obj)
    case obj@MashObject(_, Some(PermissionsSectionClass)) ⇒ PermissionsPrinter.permissionsSectionString(obj)
    case MashNumber(n, Some(BytesClass))                  ⇒ BytesPrinter.humanReadable(n)
    case MashNumber(n, Some(MillisecondsClass))           ⇒ NumberUtils.prettyString(n) + "ms"
    case MashNumber(n, Some(SecondsClass))                ⇒ NumberUtils.prettyString(n) + "s"
    case MashNumber(n, _)                                 ⇒ NumberUtils.prettyString(n)
    case MashWrapped(i: Instant) if viewConfig.fuzzyTime  ⇒ Printer.prettyTime.format(Date.from(i))
    case MashWrapped(i: Instant)                          ⇒ dateTimeFormatter.format(ZonedDateTime.ofInstant(i, ZoneId.systemDefault))
    case xs: MashList if inCell                           ⇒ xs.elements.map(renderField(_, inCell = inCell)).mkString(", ")
    case xs: MashList                                     ⇒ xs.elements.map(renderField(_, inCell = inCell)).mkString("[", ", ", "]")
    case _                                                ⇒
      val s = ToStringifier.safeStringify(value)
      if (inCell) Printer.replaceProblematicChars(s) else s
  }

}

case class PrintResult(displayModelOpt: Option[DisplayModel] = None)

class Printer(output: PrintStream, terminalSize: Dimensions, viewConfig: ViewConfig = ViewConfig()) {

  private val helpPrinter = new HelpPrinter(output)
  private val fieldRenderer = new FieldRenderer(viewConfig)

  case class PrintConfig(disableCustomViews: Boolean = false,
                         alwaysUseBrowser: Boolean = false,
                         alwaysUseTree: Boolean = false,
                         printTree: Boolean = false)

  private def done = PrintResult()

  private def browse(model: DisplayModel) = PrintResult(Some(model))

  def printOrBrowse(value: MashValue,
                    printConfig: PrintConfig = PrintConfig()): PrintResult =
    if (printConfig.alwaysUseBrowser) {
      val model = DisplayModel.getDisplayModel(value, viewConfig, terminalSize)
      PrintResult(Some(model))
    } else if (printConfig.printTree) {
      new ObjectTreePrinter(output, terminalSize, viewConfig).print(value)
      done
    } else {
      value match {
        case _: MashList | _: MashObject if printConfig.alwaysUseTree                                    ⇒
          val model = new ObjectTreeModelCreator(viewConfig).create(value)
          browse(model)
        case _ if isSuitableForTwoDTable(value)                                                          ⇒
          printOrBrowseTwoD(value)
        case xs: MashList if xs.nonEmpty && xs.forall(x ⇒ x.isAString || x.isNull)                       ⇒
          printOrBrowseTextLines(xs)
        case f: MashFunction if !printConfig.disableCustomViews                                          ⇒
          printOrBrowseHelp(f)
        case klass: MashClass if !printConfig.disableCustomViews                                         ⇒
          printOrBrowseHelp(klass)
        case obj: MashObject if obj.classOpt == Some(MethodHelpClass)                                    ⇒
          printOrBrowseHelp(obj)
        case obj: MashObject if obj.classOpt == Some(FieldHelpClass)                                     ⇒
          printOrBrowseHelp(obj)
        case obj: MashObject if obj.classOpt == Some(ViewClass)                                          ⇒
          printOrBrowseWithView(obj)
        case obj: MashObject if obj.classOpt == Some(MethodHelpClass) && !printConfig.disableCustomViews ⇒
          helpPrinter.printMethodHelp(obj)
          done
        case obj: MashObject if obj.classOpt == Some(FieldHelpClass) && !printConfig.disableCustomViews  ⇒
          helpPrinter.printFieldHelp(obj)
          done
        case obj: MashObject if obj.classOpt == Some(StatusClass) && !printConfig.disableCustomViews     ⇒
          new GitStatusPrinter(output).print(obj)
          done
        case obj: MashObject if obj.nonEmpty                                                             ⇒
          printOrBrowseOneD(obj)
        case xs: MashList if xs.nonEmpty && xs.forall(_ == ((): Unit))                                   ⇒
          done // Don't print out sequence of unit
        case method: BoundMethod if !printConfig.disableCustomViews                                      ⇒
          printOrBrowse(HelpCreator.getHelp(method), printConfig)
        case MashUnit                                                                                    ⇒
          // Don't print out Unit
          done
        case _                                                                                           ⇒
          output.println(fieldRenderer.renderField(value))
          done
      }
    }

  private def printOrBrowseWithView(obj: MashObject): PrintResult = {
    val view = ViewClass.Wrapper(obj)
    val printConfig = PrintConfig(
      disableCustomViews = view.disableCustomViews,
      alwaysUseBrowser = view.useBrowser,
      alwaysUseTree = view.useTree,
      printTree = view.printTree)
    printOrBrowse(view.data, printConfig)
  }

  private def printOrBrowseTextLines(xs: MashList): PrintResult =
    if (xs.length > terminalSize.rows - 1 && viewConfig.browseLargeOutput) {
      val model = new TextLinesModelCreator(viewConfig).create(xs)
      browse(model)
    } else {
      xs.elements.foreach(output.println)
      done
    }

  private def printOrBrowseHelp(obj: MashObject): PrintResult = {
    val model =
      obj.classOpt match {
        case Some(MethodHelpClass) ⇒ new HelpModelCreator(terminalSize, viewConfig).createForMethod(obj)
        case Some(FieldHelpClass)  ⇒ new HelpModelCreator(terminalSize, viewConfig).createForField(obj)
        case _                     ⇒ throw new IllegalArgumentException(s"Unknown help object: $obj")
      }
    printOrBrowseHelp(model)
  }

  private def printOrBrowseHelp(klass: MashClass): PrintResult = {
    val model = new HelpModelCreator(terminalSize, viewConfig).createForClass(klass)
    printOrBrowseHelp(model)
  }

  private def printOrBrowseHelp(f: MashFunction): PrintResult = {
    val model = new HelpModelCreator(terminalSize, viewConfig).createForFunction(f)
    printOrBrowseHelp(model)
  }

  private def printOrBrowseHelp(model: HelpModel): PrintResult = {
    val tooBig = model.lines.size > terminalSize.rows - 1
    if (tooBig && viewConfig.browseLargeOutput)
      browse(model)
    else {
      for (line ← model.lines)
        output.println(StyledStringDrawer.drawStyledChars(line))
      done
    }
  }

  private def printOrBrowseOneD(obj: MashObject): PrintResult = {
    val size = obj.size
    val nonDataRows = 2 // 1 header rows + 1 footer
    if (size > terminalSize.rows - nonDataRows - 1 && viewConfig.browseLargeOutput) {
      val model = new SingleObjectTableModelCreator(terminalSize, supportMarking = true, viewConfig).create(obj)
      browse(model)
    } else {
      new SingleObjectTablePrinter(output, terminalSize, viewConfig).printObject(obj)
      done
    }
  }

  private def printOrBrowseTwoD(value: MashValue): PrintResult = {
    val size = value match {
      case xs: MashList    ⇒ xs.size
      case obj: MashObject ⇒ obj.size
    }
    val nonDataRows = 4 // 3 header rows + 1 footer
    if (size > terminalSize.rows - nonDataRows - 1 && viewConfig.browseLargeOutput) {
      val model = new TwoDTableModelCreator(terminalSize, supportMarking = true, viewConfig).create(value)
      browse(model)
    } else {
      new TwoDTablePrinter(output, terminalSize, viewConfig).printTable(value)
      done
    }
  }

}