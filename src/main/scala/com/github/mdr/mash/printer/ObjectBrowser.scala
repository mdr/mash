package com.github.mdr.mash.printer

import scala.annotation.tailrec

import com.github.mdr.mash.InputAction
import com.github.mdr.mash.InputSequence._
import com.github.mdr.mash.Key._
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.KeyMap
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.screen.Point
import com.github.mdr.mash.screen.Screen
import com.github.mdr.mash.screen.Style
import com.github.mdr.mash.screen.StyledCharacter
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils

import ObjectBrowserActions._

object ObjectBrowserActions {

  case object NextPage extends InputAction
  case object PreviousPage extends InputAction
  case object NextItem extends InputAction
  case object PreviousItem extends InputAction
  case object FirstItem extends InputAction
  case object LastItem extends InputAction
  case object ExitBrowser extends InputAction
  case object NavigateRight extends InputAction
  case object AcceptCompletion extends InputAction

}

object ObjectBrowser {

  def launch(model: ObjectTableModel, terminalInfo: TerminalInfo) {
    val columns = terminalInfo.columns
    val objectBrowser = new ObjectBrowser(model, terminalInfo)
    objectBrowser.inputLoop()
  }

}

class ObjectBrowser(model: ObjectTableModel, terminalInfo: TerminalInfo) {

  private var currentRow = 0
  private var firstRow = 0
  private var previousScreenOpt: Option[Screen] = None
  private var continue = true
  private val boxCharacterSupplier: BoxCharacterSupplier = UnicodeBoxCharacterSupplier

  private def draw() {
    val screen = render
    val output = screen.draw(previousScreenOpt, terminalInfo.columns)
    previousScreenOpt = Some(screen)
    System.out.write(output.getBytes)
    System.out.flush()
  }

  private def windowSize = terminalInfo.rows - 5 // three header rows, a footer row, a status line

  private def render: Screen = {
    val objectTablePrinter = new ObjectTablePrinter(terminalInfo)
    def simpleLine(s: String) = Line(s.map(StyledCharacter(_)))
    val headerLines: Seq[Line] = Seq(
      objectTablePrinter.renderTopRow(model),
      objectTablePrinter.renderHeaderRow(model),
      objectTablePrinter.renderBelowHeaderRow(model)).map(simpleLine)

    val footerLine = simpleLine(objectTablePrinter.renderBottomRow(model))
    val countChars = s"${currentRow + 1}/${model.objects.size}".map(StyledCharacter(_, Style(inverse = true)))
    val keyChars = " (".map(StyledCharacter(_)) ++ "q".map(StyledCharacter(_, Style(inverse = true))) ++ " to exit)".map(StyledCharacter(_))
    val statusLine = Line(countChars ++ keyChars)
    val footerLines = Seq(footerLine, statusLine)

    val currentRowRelative = currentRow - firstRow
    val objects = model.objects.drop(firstRow).take(windowSize)
    def renderObject(obj: Map[String, String], isSelected: Boolean): Line = {
      val side = boxCharacterSupplier.doubleVertical.map(StyledCharacter(_))
      val internalVertical = boxCharacterSupplier.singleVertical.map(StyledCharacter(_, Style(inverse = isSelected)))
      def renderCell(name: String) = StringUtils.fitToWidth(obj(name), model.columnWidth(name)).map(StyledCharacter(_, Style(inverse = isSelected)))
      val innerChars = Utils.intercalate(model.columnNames.map(renderCell), internalVertical)
      Line(side ++ innerChars ++ side)
    }
    val dataLines =
      for {
        (obj, i) ← objects.zipWithIndex
      } yield renderObject(obj, i == currentRowRelative)

    Screen(headerLines ++ dataLines ++ footerLines, Point(0, 0), cursorVisible = false)
  }

  @tailrec
  private def inputLoop() {
    draw()
    val action = InputAction.fetchAction(false, ObjectBrowserKeyMap)
    handleAction(action)
    if (continue)
      inputLoop()
    else
      for (screen ← previousScreenOpt) {
        System.out.write(screen.acceptScreen.getBytes)
        System.out.flush()
      }
  }

  private def adjustWindowToFit() {
    val delta = currentRow - (firstRow + windowSize - 1)
    if (delta >= 0)
      firstRow += delta
    val delta2 = firstRow - currentRow
    if (delta2 >= 0)
      firstRow -= delta2
  }

  private def handleAction(action: InputAction) = action match {
    case NextItem ⇒
      if (currentRow < model.objects.size - 1) {
        currentRow += 1
        adjustWindowToFit()
        draw()
      }
    case NextPage ⇒
      currentRow += windowSize - 1
      currentRow = math.min(model.objects.size - 1, currentRow)
      adjustWindowToFit()
      draw()
    case PreviousItem ⇒
      if (currentRow > 0) {
        currentRow -= 1
        adjustWindowToFit()
        draw()
      }
    case PreviousPage ⇒
      currentRow -= windowSize - 1
      currentRow = math.max(0, currentRow)
      adjustWindowToFit()
      draw()
    case ExitBrowser ⇒
      continue = false
    case FirstItem ⇒
      currentRow = 0
      adjustWindowToFit()
      draw()
    case LastItem ⇒
      currentRow = model.objects.size - 1
      adjustWindowToFit()
      draw()
    case _ ⇒
  }

}

object ObjectBrowserKeyMap extends KeyMap(Map(
  KeyPress(Down) -> NextItem,
  KeyPress(BasicKey('n'), control = true) -> NextItem,
  KeyPress(Up) -> PreviousItem,
  KeyPress(BasicKey('p'), control = true) -> PreviousItem,
  KeyPress(PageUp) -> PreviousPage,
  KeyPress(PageDown) -> NextPage,
  OtherSequence(" ") -> NextPage,
  OtherSequence("q") -> ExitBrowser,
  OtherSequence("g") -> FirstItem,
  OtherSequence("G") -> LastItem))

