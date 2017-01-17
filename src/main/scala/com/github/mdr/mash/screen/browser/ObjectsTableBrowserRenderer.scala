package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.printer.model.{ ObjectTableRow, ObjectsTableModel }
import com.github.mdr.mash.printer.{ ObjectsTableStringifier, UnicodeBoxCharacterSupplier }
import com.github.mdr.mash.repl.browser.ObjectsTableBrowserState
import com.github.mdr.mash.repl.browser.ObjectsTableBrowserState.SearchState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.{ Colour, KeyHint, _ }
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.Utils.tupled
import com.github.mdr.mash.utils.{ StringUtils, Utils }

import scala.collection.mutable.ArrayBuffer

class ObjectsTableBrowserRenderer(state: ObjectsTableBrowserState, terminalInfo: TerminalInfo)
  extends AbstractBrowserRenderer(state, terminalInfo) {

  private val boxCharacterSupplier = UnicodeBoxCharacterSupplier
  private val objectTableStringifier = new ObjectsTableStringifier(showSelections = true)

  protected def renderLines: Seq[Line] = {
    val upperStatusLine = renderUpperStatusLine
    val headerLines = renderHeaderLines
    val dataLines = renderDataLines
    val footerLine = renderFooterLine
    val statusLine = renderStatusLine
    Seq(upperStatusLine) ++ headerLines ++ dataLines ++ Seq(footerLine, statusLine)
  }

  private def renderHeaderRow(model: ObjectsTableModel): Line = {
    def renderColumn(name: String): Seq[StyledCharacter] = {
      val fit = StringUtils.fitToWidth(name, model.columnWidth(name))
      fit.style(Style(bold = true, foregroundColour = Colour.Yellow))
    }
    val buffer = ArrayBuffer[StyledCharacter]()
    buffer ++= boxCharacterSupplier.doubleVertical.style
    buffer ++= (" " + boxCharacterSupplier.singleVertical).style
    buffer ++= Utils.intercalate(model.columnNames.map(renderColumn), boxCharacterSupplier.singleVertical.style)
    buffer ++= boxCharacterSupplier.doubleVertical.style
    Line(buffer)
  }

  private def renderHeaderLines: Seq[Line] =
    Seq(
      Line(objectTableStringifier.renderTopRow(model).style),
      renderHeaderRow(model),
      Line(objectTableStringifier.renderBelowHeaderRow(model).style))

  private def renderDataLines: Seq[Line] = {
    val objects = model.objects.drop(state.firstRow).take(windowSize)
    for {
      (obj, i) ← objects.zipWithIndex
      actualIndex = i + state.firstRow
    } yield renderObject(obj, actualIndex == state.selectedRow, state.currentColumnOpt, state.markedRows contains actualIndex, actualIndex)
  }

  private def renderObject(obj: ObjectTableRow, isCursorRow: Boolean, currentColumnOpt: Option[Int], isSelected: Boolean, row: Int): Line = {
    val side = boxCharacterSupplier.doubleVertical.style
    val selectedChar = if (isSelected) "◈" else " "
    val highlightRow = isCursorRow && currentColumnOpt.isEmpty
    val selected = (selectedChar + boxCharacterSupplier.singleVertical).style(Style(inverse = highlightRow))
    val internalVertical = boxCharacterSupplier.singleVertical.style(Style(inverse = highlightRow))

    def renderCell(name: String, column: Int): Seq[StyledCharacter] = {
      val searchInfoOpt = state.searchStateOpt.flatMap(_.byPoint.get(Point(row, column)))
      val highlightCell = isCursorRow && currentColumnOpt.forall(_ == column)
      val cellContents = StringUtils.fitToWidth(obj.data(name), model.columnWidth(name))
      val buf = ArrayBuffer[StyledCharacter]()
      for ((c, offset) <- cellContents.zipWithIndex) {
        val isSearchMatch = searchInfoOpt exists (_.matches exists (_ contains offset))
        val style = Style(inverse = highlightCell, bold = isSearchMatch, foregroundColour = if (isSearchMatch) Colour.Cyan else Colour.Default)
        buf += StyledCharacter(c, style)
      }
      buf
    }

    val renderedCells = model.columnNames.zipWithIndex.map(tupled(renderCell))
    val innerChars = Utils.intercalate(renderedCells, internalVertical)
    Line(side ++ selected ++ innerChars ++ side)
  }

  private def renderFooterLine = Line(objectTableStringifier.renderBottomRow(model).style)

  private def renderRegularStatusLine: Line = {
    import KeyHint._
    val hints = Seq(Exit, Mark, Focus, Back, Insert, InsertWhole, Tree, Search, Expression) ++
      state.currentColumnOpt.toSeq.flatMap(_ ⇒ Seq(Row, HideColumn))
    val countChars = s"${currentRow + 1}/${model.objects.size}".style(Style(inverse = true))
    Line(countChars ++ " (".style ++ renderKeyHints(hints) ++ ")".style)
  }

  private def renderExpressionInputStatusLine(expression: String): Line = {
    import KeyHint._
    val hints = Seq(DoneSearch)
    Line("(".style ++ renderKeyHints(hints) ++ ")".style)
  }

  private def renderIncrementalSearchStatusLine(searchState: SearchState): Line = {
    import KeyHint._
    val hints = Seq(NextHit, PreviousHit, DoneSearch, if (searchState.ignoreCase) CaseSensitive else CaseInsensitive)
    val hits = searchState.rows
    val currentHit = hits.indexOf(currentRow)
    val countChars = s"${currentHit + 1}/${hits.size}".style(Style(inverse = true))
    Line(countChars ++ s" Find: ${searchState.query}".style ++ " (".style ++ renderKeyHints(hints) ++ ")".style)
  }

  private def renderStatusLine: Line =
    state.searchStateOpt match {
      case Some(searchState) ⇒ renderIncrementalSearchStatusLine(searchState)
      case None              ⇒
        state.expressionOpt match {
          case Some(expression) ⇒ renderExpressionInputStatusLine(expression)
          case None             ⇒ renderRegularStatusLine
        }
    }

  private def model = state.model

  private def currentRow = state.selectedRow

  protected val windowSize = state.windowSize(terminalInfo.rows)

}