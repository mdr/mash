package com.github.mdr.mash.render.browser

import com.github.mdr.mash.printer.UnicodeBoxCharacterSupplier
import com.github.mdr.mash.printer.model.SingleObjectTableModel
import com.github.mdr.mash.repl.browser.SearchState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen._
import com.github.mdr.mash.render.browser.ArrowHelper._
import com.github.mdr.mash.utils.StringUtils.{ centre, ellipsisise }
import com.github.mdr.mash.utils.Utils._
import com.github.mdr.mash.utils.{ Point, Region, StringUtils }

import scala.collection.mutable.ArrayBuffer

class SingleObjectTableCommonRenderer(model: SingleObjectTableModel,
                                      markedRowsOpt: Option[Set[Int]] = None,
                                      selectedIndexOpt: Option[Int] = None,
                                      searchStateOpt: Option[SearchState] = None) {

  import UnicodeBoxCharacterSupplier._

  def renderTableLines(rowOffset: Int = 0, rowCount: Int = model.numberOfRows): Seq[Line] = {
    val moreDataItemsBelowWindow = rowOffset + rowCount < model.numberOfRows
    val headerLines = renderHeaderLines(moreDataItemsAboveWindow = rowOffset > 0)
    val fieldLines = renderFieldLines(rowOffset, rowCount)
    val footerLine = renderFooterLine(addArrow = moreDataItemsBelowWindow)
    headerLines ++ fieldLines ++ Seq(footerLine)
  }

  private def renderHeaderLines(moreDataItemsAboveWindow: Boolean): Seq[Line] =
    model.classNameOpt match {
      case Some(className) ⇒ renderClassHeaderLines(className, moreDataItemsAboveWindow)
      case None            ⇒ Seq(renderTopRow(break = true, addArrow = moreDataItemsAboveWindow))
    }

  private def renderClassHeaderLines(className: String, moreDataItemsAboveWindow: Boolean): Seq[Line] = {
    val topLine = renderTopRow(break = false, addArrow = false)
    val classNameLine = renderClassNameLine(className)
    val belowHeaderLine = renderBelowHeaderLine(addArrow = moreDataItemsAboveWindow)
    Seq(topLine, classNameLine, belowHeaderLine)
  }

  /**
    * ╔══════════════════════════╗
    * or
    * ╔════════════╤═════════════╗
    * or
    * ╔═╤══════════╤═════════════╗
    */
  private def renderTopRow(break: Boolean, addArrow: Boolean): Line = {
    val internalColumn = if (break) doubleHorizontalSingleDown else doubleHorizontal
    renderBorderRow(doubleTopLeft, doubleHorizontal, internalColumn, doubleTopRight, downArrow = addArrow)
  }

  /**
    * ║           Class          ║
    **/
  private def renderClassNameLine(className: String): Line = {
    val fullRowWidth = model.fieldColumnWidth + model.valueColumnWidth + (if (showMarkedRows) 3 else 1)
    val fit = ellipsisise(centre(className, fullRowWidth), fullRowWidth)
    val renderedClassName = fit.style(classNameStyle)
    Line(doubleVertical.style + renderedClassName + doubleVertical.style)
  }

  /**
    * ╟────────────┬─────────────╢
    * or
    * ╟─┬──────────┬─────────────╢
    */
  private def renderBelowHeaderLine(addArrow: Boolean): Line =
    renderBorderRow(doubleVerticalSingleRight, singleHorizontal, singleHorizontalSingleDown, doubleVerticalSingleLeft, downArrow = addArrow)

  private def renderFieldLines(rowOffset: Int, rowCount: Int): Seq[Line] = {
    def getMatchRegions(rowIndex: Int, columnIndex: Int): Seq[Region] = {
      val cellLocation = Point(rowIndex, columnIndex)
      searchStateOpt.flatMap(_.getCellSearchInfo(cellLocation)).map(_.matches).getOrElse(Seq())
    }
    for {
      ((renderedField, renderedValue), rowIndex) ← model.fields.toSeq.zipWithIndex.window(rowOffset, rowCount)
      isCursorRow = selectedIndexOpt contains rowIndex
      isMarked = markedRowsOpt exists (_ contains rowIndex)
      fieldSearchHitRegions = getMatchRegions(rowIndex, 0)
      valueSearchHitRegions = getMatchRegions(rowIndex, 1)
    } yield renderFieldLine(renderedField, renderedValue, isCursorRow, isMarked, fieldSearchHitRegions, valueSearchHitRegions)
  }

  /**
    * ║fieldName   │fieldValue  ║
    * or
    * ║◈│fieldName │fieldValue  ║
    */
  private def renderFieldLine(renderedField: String,
                              renderedValue: String,
                              isCursorRow: Boolean,
                              isMarked: Boolean,
                              fieldSearchHitRegions: Seq[Region],
                              valueSearchHitRegions: Seq[Region]): Line = {
    val side = doubleVertical.style
    val markCell: StyledString =
      if (showMarkedRows) {
        val markChar = if (isMarked) "◈" else " "
        (markChar + singleVertical).style(getStyle(highlight = isCursorRow))
      } else
        StyledString.Empty
    val internalVertical = singleVertical.style(getStyle(highlight = isCursorRow))
    val fieldChars = renderFieldCell(renderedField, isCursorRow, fieldSearchHitRegions)
    val valueChars = renderValueCell(renderedValue, isCursorRow, valueSearchHitRegions)
    Line(side + markCell + fieldChars + internalVertical + valueChars + side)
  }

  private def renderFieldCell(renderedField: String, isCursorRow: Boolean, searchHitRegions: Seq[Region]): StyledString = {
    val chars = StringUtils.fitToWidth(renderedField, model.fieldColumnWidth)
    val buf = ArrayBuffer[StyledCharacter]()
    for ((c, offset) ← chars.zipWithIndex) {
      val isSearchMatch = searchHitRegions exists (_ contains offset)
      val style = getStyle(highlight = isCursorRow, isLabel = true, isSearchMatch = isSearchMatch)
      buf += StyledCharacter(c, style)
    }
    StyledString(buf)
  }

  private def renderValueCell(renderedValue: String, isCursorRow: Boolean, searchHitRegions: Seq[Region]): StyledString = {
    val chars = StringUtils.fitToWidth(renderedValue, model.valueColumnWidth)
    val buf = ArrayBuffer[StyledCharacter]()
    for ((c, offset) ← chars.zipWithIndex) {
      val isSearchMatch = searchHitRegions exists (_ contains offset)
      val style = getStyle(highlight = isCursorRow, isSearchMatch = isSearchMatch)
      buf += StyledCharacter(c, style)
    }
    StyledString(buf)
  }

  /**
    * ╚════════════╧═════════════╝
    * or
    * ╚═╧══════════╧═════════════╝
    */
  private def renderFooterLine(addArrow: Boolean): Line =
    renderBorderRow(doubleBottomLeft, doubleHorizontal, doubleHorizontalSingleUp, doubleBottomRight, downArrow = addArrow)

  private val classNameStyle: Style = Style(bold = true, foregroundColour = BasicColour.Yellow)

  private def getStyle(highlight: Boolean = false, isSearchMatch: Boolean = false, isLabel: Boolean = false): Style = {
    val foregroundColour =
      if (isSearchMatch) BasicColour.Cyan
      else if (isLabel) BasicColour.Yellow
      else DefaultColour
    Style(inverse = highlight, bold = isSearchMatch, foregroundColour = foregroundColour)
  }

  private def renderBorderRow(first: String,
                              internal: String,
                              internalColumn: String,
                              last: String,
                              downArrow: Boolean = false,
                              upArrow: Boolean = false): Line = {
    val sb = new StringBuilder
    sb.append(first)
    if (showMarkedRows)
      sb.append(internal + internalColumn)
    sb.append(internal * model.fieldColumnWidth)
      .append(internalColumn)
      .append(internal * model.valueColumnWidth)
      .append(last)
    val chars = sb.toString.when(downArrow, addDownArrow).when(upArrow, addUpArrow)
    Line(chars.style)
  }

  private def showMarkedRows = markedRowsOpt.isDefined

}
