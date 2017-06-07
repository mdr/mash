package com.github.mdr.mash.screen.browser

import com.github.mdr.mash.printer.model.TwoDTableModel
import com.github.mdr.mash.printer.{ ColumnId, TwoDTableStringifier, UnicodeBoxCharacterSupplier }
import com.github.mdr.mash.repl.browser.TwoDTableBrowserState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.{ KeyHint, _ }
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils.tupled

import scala.collection.mutable.ArrayBuffer

class TwoDTableBrowserRenderer(state: TwoDTableBrowserState, terminalInfo: TerminalInfo)
  extends AbstractBrowserRenderer(state, terminalInfo) {

  private val boxCharacterSupplier = UnicodeBoxCharacterSupplier
  private val objectTableStringifier = new TwoDTableStringifier(showSelections = true)

  protected def renderLines: Seq[Line] = {
    val upperStatusLine = renderUpperStatusLine
    val headerLines = renderHeaderLines
    val dataLines = renderDataLines
    val footerLine = renderFooterLine
    val statusLine = renderStatusLine
    Seq(upperStatusLine) ++ headerLines ++ dataLines ++ Seq(footerLine, statusLine)
  }

  private def renderHeaderRow(model: TwoDTableModel): Line = {
    def renderColumn(columnId: ColumnId): StyledString = {
      val fit = StringUtils.fitToWidth(model.columnName(columnId), model.columnWidth(columnId))
      fit.style(Style(bold = true, foregroundColour = BasicColour.Yellow))
    }
    val buffer = ArrayBuffer[StyledCharacter]()
    buffer ++= boxCharacterSupplier.doubleVertical.style.chars
    buffer ++= (" " + boxCharacterSupplier.singleVertical).style.chars
    buffer ++= StyledString.mkString(model.columnIds.map(renderColumn), boxCharacterSupplier.singleVertical.style).chars
    buffer ++= boxCharacterSupplier.doubleVertical.style.chars
    Line(StyledString(buffer))
  }

  private def renderHeaderLines: Seq[Line] =
    Seq(
      Line(objectTableStringifier.renderTopRow(model).style),
      renderHeaderRow(model),
      Line(objectTableStringifier.renderBelowHeaderRow(model).style))

  private def renderDataLines: Seq[Line] = {
    val rows = model.rows.drop(state.firstRow).take(windowSize)
    for {
      (row, i) ← rows.zipWithIndex
      actualIndex = i + state.firstRow
    } yield renderObject(row, actualIndex == state.selectedRow, state.currentColumnOpt, state.markedRows contains actualIndex, actualIndex)
  }

  private def renderObject(row: TwoDTableModel.Row,
                           isCursorRow: Boolean,
                           currentColumnIndexOpt: Option[Int],
                           isSelected: Boolean,
                           rowIndex: Int): Line = {
    val side = boxCharacterSupplier.doubleVertical.style
    val selectedChar = if (isSelected) "◈" else " "
    val highlightRow = isCursorRow && currentColumnIndexOpt.isEmpty
    val selected = (selectedChar + boxCharacterSupplier.singleVertical).style(Style(inverse = highlightRow))
    val internalVertical = boxCharacterSupplier.singleVertical.style(Style(inverse = highlightRow))

    def renderCell(columnId: ColumnId, columnIndex: Int): StyledString = {
      val searchInfoOpt = state.searchStateOpt.flatMap(_.byPoint.get(Point(rowIndex, columnIndex)))
      val highlightCell = isCursorRow && currentColumnIndexOpt.forall(_ == columnIndex)
      val cellContents = StringUtils.fitToWidth(row.renderedValue(columnId), model.columnWidth(columnId))
      val buf = ArrayBuffer[StyledCharacter]()
      for ((c, offset) <- cellContents.zipWithIndex) {
        val isSearchMatch = searchInfoOpt exists (_.matches exists (_ contains offset))
        val foregroundColour = if (isSearchMatch) BasicColour.Cyan else DefaultColour
        val style = Style(inverse = highlightCell, bold = isSearchMatch, foregroundColour = foregroundColour)
        buf += StyledCharacter(c, style)
      }
      StyledString(buf)
    }

    val renderedCells = model.columnIds.zipWithIndex.map(tupled(renderCell))
    val innerChars = StyledString.mkString(renderedCells, internalVertical)
    Line(side + selected + innerChars + side)
  }

  private def renderFooterLine = Line(objectTableStringifier.renderBottomRow(model).style)

  private def renderRegularStatusLine: Line = {
    import KeyHint._
    val hints = Seq(Exit, Mark, Focus, Back, Insert, InsertWhole, Tree, Search, Expression, Children) ++
      state.currentColumnOpt.toSeq.flatMap(_ ⇒ Seq(Row, HideColumn))
    val countChars = s"${currentRow + 1}/${model.rows.size}".style(Style(inverse = true))
    Line(countChars + " (".style + renderKeyHints(hints) + ")".style)
  }

  private def renderStatusLine: Line =
    state.searchStateOpt match {
      case Some(searchState) ⇒ StatusLineRenderers.renderIncrementalSearchStatusLine(currentRow, searchState)
      case None              ⇒
        state.expressionOpt match {
          case Some(expression) ⇒ StatusLineRenderers.renderExpressionInputStatusLine(expression)
          case None             ⇒ renderRegularStatusLine
        }
    }

  private def model = state.model

  private def currentRow = state.selectedRow

  protected val windowSize = state.windowSize(terminalInfo.rows)

}