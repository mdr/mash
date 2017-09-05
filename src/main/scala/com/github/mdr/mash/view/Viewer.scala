package com.github.mdr.mash.view

import java.io.PrintStream

import com.github.mdr.mash.classes.{ BoundMethod, MashClass }
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.core.help._
import com.github.mdr.mash.ns.git.StatusClass
import com.github.mdr.mash.ns.view.ViewClass
import com.github.mdr.mash.view.printer._
import com.github.mdr.mash.view.model.TwoDTableModelCreator.isSuitableForTwoDTable
import com.github.mdr.mash.view.model._
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.screen.StyledStringDrawer
import com.github.mdr.mash.utils.Dimensions
import com.github.mdr.mash.view.common.ObjectTreeCommonRenderer
import org.ocpsoft.prettytime.PrettyTime

case class ViewConfig(fuzzyTime: Boolean = true,
                      browseLargeOutput: Boolean = true)

case class ViewDirectives(disableCustomViews: Boolean = false,
                          alwaysUseBrowser: Boolean = false,
                          alwaysUseTree: Boolean = false,
                          alwaysPrint: Boolean = false)

case class ViewResult(displayModelOpt: Option[DisplayModel] = None)

object Viewer {

  val prettyTime = new PrettyTime

  def replaceProblematicChars(s: String): String = s.map {
    case '\t' | '\n' | '\r' | '\b' ⇒ ' '
    case c                         ⇒ c
  }

}

case class Viewer(output: PrintStream,
                  terminalSize: Dimensions,
                  viewConfig: ViewConfig = ViewConfig(),
                  viewDirectives: ViewDirectives = ViewDirectives()) {

  import viewConfig._
  import viewDirectives._

  private val fieldRenderer = new FieldRenderer(viewConfig)

  private def done = ViewResult()

  private def browse(model: DisplayModel) = ViewResult(Some(model))

  def view(value: MashValue): ViewResult =
    if (alwaysUseBrowser) {
      val model = DisplayModel.getDisplayModel(value, viewConfig, terminalSize)
      browse(model)
    } else
      value match {
        case _ if alwaysUseTree                                                              ⇒
          viewTree(value)
        case _ if isSuitableForTwoDTable(value)                                              ⇒
          view2D(value)
        case xs: MashList if xs.nonEmpty && xs.forall(x ⇒ x.isAString || x.isNull)           ⇒
          viewTextLines(xs)
        case f: MashFunction if !disableCustomViews                                          ⇒
          viewHelp(f)
        case method: BoundMethod if !disableCustomViews                                      ⇒
          view(HelpCreator.getHelp(method))
        case klass: MashClass if !disableCustomViews                                         ⇒
          viewHelp(klass)
        case obj: MashObject if obj.classOpt == Some(MethodHelpClass) && !disableCustomViews ⇒
          viewHelp(obj)
        case obj: MashObject if obj.classOpt == Some(FieldHelpClass) && !disableCustomViews  ⇒
          viewHelp(obj)
        case obj: MashObject if obj.classOpt == Some(ViewClass)                              ⇒
          viewWithDirectives(obj)
        case obj: MashObject if obj.classOpt == Some(StatusClass) && !disableCustomViews     ⇒
          printGitStatus(obj)
        case obj: MashObject if obj.nonEmpty                                                 ⇒
          viewSingleObject(obj)
        case xs: MashList if xs.nonEmpty && xs.forall(_ == ((): Unit))                       ⇒
          done // Don't print out sequence of unit
        case MashUnit                                                                        ⇒
          // Don't print out Unit
          done
        case _                                                                               ⇒
          output.println(fieldRenderer.renderField(value))
          done
      }

  private def printGitStatus(obj: MashObject) = {
    new GitStatusPrinter(output).print(obj)
    done
  }

  private def viewTree(value: MashValue) = {
    val model = new ObjectTreeModelCreator(viewConfig).create(value)
    val commonRenderer = new ObjectTreeCommonRenderer(model, selectionPathOpt = None, terminalSize)
    val lines = commonRenderer.renderTableLines
    val tooBig = lines.size > terminalSize.rows - 1
    if (tooBig && browseLargeOutput && !alwaysPrint)
      browse(model)
    else {
      for (line ← lines)
        output.println(StyledStringDrawer.drawStyledChars(line.string))
      done
    }
  }

  private def viewWithDirectives(obj: MashObject): ViewResult = {
    val viewConfig = ViewClass.Wrapper(obj)
    val viewDirectives = ViewDirectives(
      disableCustomViews = viewConfig.disableCustomViews,
      alwaysUseBrowser = viewConfig.useBrowser,
      alwaysUseTree = viewConfig.useTree,
      alwaysPrint = viewConfig.print)
    copy(viewDirectives = viewDirectives).view(viewConfig.data)
  }

  private def viewTextLines(xs: MashList): ViewResult = {
    val tooBig = xs.length > terminalSize.rows - 1
    if (tooBig && browseLargeOutput && !alwaysPrint) {
      val model = new TextLinesModelCreator(viewConfig).create(xs)
      browse(model)
    } else {
      xs.elements.foreach(output.println)
      done
    }
  }

  private def viewHelp(obj: MashObject): ViewResult = {
    val model =
      obj.classOpt match {
        case Some(MethodHelpClass) ⇒ new HelpModelCreator(terminalSize, viewConfig).createForMethod(obj)
        case Some(FieldHelpClass)  ⇒ new HelpModelCreator(terminalSize, viewConfig).createForField(obj)
        case _                     ⇒ throw new IllegalArgumentException(s"Unknown help object: $obj")
      }
    viewHelp(model)
  }

  private def viewHelp(klass: MashClass): ViewResult = {
    val model = new HelpModelCreator(terminalSize, viewConfig).createForClass(klass)
    viewHelp(model)
  }

  private def viewHelp(f: MashFunction): ViewResult = {
    val model = new HelpModelCreator(terminalSize, viewConfig).createForFunction(f)
    viewHelp(model)
  }

  private def viewHelp(model: HelpModel): ViewResult = {
    val tooBig = model.lines.size > terminalSize.rows - 1
    if (tooBig && browseLargeOutput && !alwaysPrint)
      browse(model)
    else {
      for (line ← model.lines)
        output.println(StyledStringDrawer.drawStyledChars(line))
      done
    }
  }

  private def viewSingleObject(obj: MashObject): ViewResult = {
    val size = obj.size
    val nonDataRows = 2 // 1 header rows + 1 footer
    val tooBig = size > terminalSize.rows - nonDataRows - 1
    if (tooBig && browseLargeOutput && !alwaysPrint) {
      val model = new SingleObjectTableModelCreator(terminalSize, supportMarking = true, viewConfig).create(obj)
      browse(model)
    } else {
      new SingleObjectTablePrinter(output, terminalSize, viewConfig).printObject(obj)
      done
    }
  }

  private def view2D(value: MashValue): ViewResult = {
    val size = value match {
      case xs: MashList    ⇒ xs.size
      case obj: MashObject ⇒ obj.size
    }
    val nonDataRows = 4 // 3 header rows + 1 footer
    val tooBig = size > terminalSize.rows - nonDataRows - 1
    if (tooBig && browseLargeOutput && !alwaysPrint) {
      val model = new TwoDTableModelCreator(terminalSize, supportMarking = true, viewConfig).create(value)
      browse(model)
    } else {
      new TwoDTablePrinter(output, terminalSize, viewConfig).printTable(value)
      done
    }
  }

}