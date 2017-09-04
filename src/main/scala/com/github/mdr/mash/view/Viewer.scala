package com.github.mdr.mash.view

import java.io.PrintStream

import com.github.mdr.mash.classes.{ BoundMethod, MashClass }
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.core.help._
import com.github.mdr.mash.ns.git.StatusClass
import com.github.mdr.mash.ns.view.ViewClass
import com.github.mdr.mash.printer._
import com.github.mdr.mash.view.model.TwoDTableModelCreator.isSuitableForTwoDTable
import com.github.mdr.mash.view.model._
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.screen.StyledStringDrawer
import com.github.mdr.mash.utils.Dimensions
import org.ocpsoft.prettytime.PrettyTime

case class ViewConfig(fuzzyTime: Boolean = true,
                      browseLargeOutput: Boolean = true)

object Viewer {

  val prettyTime = new PrettyTime

  def replaceProblematicChars(s: String): String = s.map {
    case '\t' | '\n' | '\r' | '\b' ⇒ ' '
    case c                         ⇒ c
  }

}

case class ViewResult(displayModelOpt: Option[DisplayModel] = None)

class Viewer(output: PrintStream, terminalSize: Dimensions, viewConfig: ViewConfig = ViewConfig()) {

  private val helpPrinter = new HelpPrinter(output)
  private val fieldRenderer = new FieldRenderer(viewConfig)

  case class PrintConfig(disableCustomViews: Boolean = false,
                         alwaysUseBrowser: Boolean = false,
                         alwaysUseTree: Boolean = false,
                         printTree: Boolean = false)

  private def done = ViewResult()

  private def browse(model: DisplayModel) = ViewResult(Some(model))

  def view(value: MashValue,
           printConfig: PrintConfig = PrintConfig()): ViewResult =
    if (printConfig.alwaysUseBrowser) {
      val model = DisplayModel.getDisplayModel(value, viewConfig, terminalSize)
      ViewResult(Some(model))
    } else if (printConfig.printTree) {
      new ObjectTreePrinter(output, terminalSize, viewConfig).print(value)
      done
    } else {
      value match {
        case _: MashList | _: MashObject if printConfig.alwaysUseTree                                    ⇒
          val model = new ObjectTreeModelCreator(viewConfig).create(value)
          browse(model)
        case _ if isSuitableForTwoDTable(value)                                                          ⇒
          view(value)
        case xs: MashList if xs.nonEmpty && xs.forall(x ⇒ x.isAString || x.isNull)                       ⇒
          printOrBrowseTextLines(xs)
        case f: MashFunction if !printConfig.disableCustomViews                                          ⇒
          view(f)
        case klass: MashClass if !printConfig.disableCustomViews                                         ⇒
          view(klass)
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
          view(obj)
        case xs: MashList if xs.nonEmpty && xs.forall(_ == ((): Unit))                                   ⇒
          done // Don't print out sequence of unit
        case method: BoundMethod if !printConfig.disableCustomViews                                      ⇒
          view(HelpCreator.getHelp(method), printConfig)
        case MashUnit                                                                                    ⇒
          // Don't print out Unit
          done
        case _                                                                                           ⇒
          output.println(fieldRenderer.renderField(value))
          done
      }
    }

  private def printOrBrowseWithView(obj: MashObject): ViewResult = {
    val viewConfig = ViewClass.Wrapper(obj)
    val printConfig = PrintConfig(
      disableCustomViews = viewConfig.disableCustomViews,
      alwaysUseBrowser = viewConfig.useBrowser,
      alwaysUseTree = viewConfig.useTree,
      printTree = viewConfig.printTree)
    view(viewConfig.data, printConfig)
  }

  private def printOrBrowseTextLines(xs: MashList): ViewResult =
    if (xs.length > terminalSize.rows - 1 && viewConfig.browseLargeOutput) {
      val model = new TextLinesModelCreator(viewConfig).create(xs)
      browse(model)
    } else {
      xs.elements.foreach(output.println)
      done
    }

  private def printOrBrowseHelp(obj: MashObject): ViewResult = {
    val model =
      obj.classOpt match {
        case Some(MethodHelpClass) ⇒ new HelpModelCreator(terminalSize, viewConfig).createForMethod(obj)
        case Some(FieldHelpClass)  ⇒ new HelpModelCreator(terminalSize, viewConfig).createForField(obj)
        case _                     ⇒ throw new IllegalArgumentException(s"Unknown help object: $obj")
      }
    view(model)
  }

  private def view(klass: MashClass): ViewResult = {
    val model = new HelpModelCreator(terminalSize, viewConfig).createForClass(klass)
    view(model)
  }

  private def view(f: MashFunction): ViewResult = {
    val model = new HelpModelCreator(terminalSize, viewConfig).createForFunction(f)
    view(model)
  }

  private def view(model: HelpModel): ViewResult = {
    val tooBig = model.lines.size > terminalSize.rows - 1
    if (tooBig && viewConfig.browseLargeOutput)
      browse(model)
    else {
      for (line ← model.lines)
        output.println(StyledStringDrawer.drawStyledChars(line))
      done
    }
  }

  private def view(obj: MashObject): ViewResult = {
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

  private def view(value: MashValue): ViewResult = {
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