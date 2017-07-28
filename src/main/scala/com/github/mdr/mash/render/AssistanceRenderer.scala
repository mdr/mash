package com.github.mdr.mash.render

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.screen.{ BasicColour, Line, Style, StyledString }
import com.github.mdr.mash.utils.{ Dimensions, StringUtils, StyledStringUtils }
import com.github.mdr.mash.screen.Style.StylableString

object AssistanceRenderer {

  private val InnerLineStart = "│ "
  private val InnerLineEnd = " │"
  private val TopLineStart = "┌─ "
  private val TopLineEnd = "─┐"
  private val BottomLineStart = "└─"
  private val BottomLineEnd = "─┘"

  private val NumberOfTopLineCharsNotUsedForTitle = TopLineStart.length + " ".length + TopLineEnd.length
  private val NumberOfInnerLineCharsNotUsedForContent = InnerLineStart.length + InnerLineEnd.length
  private val NumberOfBottomLineCharsNotUsedForFiller = BottomLineStart.length + BottomLineEnd.length

  private val TitleStyle = Style(bold = true, foregroundColour = BasicColour.Yellow)

  def render(assistanceState: AssistanceState, terminalSize: Dimensions): Seq[Line] = {
    val AssistanceState(assistable) = assistanceState
    val AssistanceLines(title, lines) = AssistanceContentGenerator.getAssistanceState(assistable)

    val widestDesiredWidth = getDesiredTopLineWidth(title) max lines.map(getDesiredInnerLineWidth).max
    val boxWidth = widestDesiredWidth min terminalSize.columns max NumberOfTopLineCharsNotUsedForTitle

    val topLine = renderTopLine(title, boxWidth)
    val innerLines = renderInnerLines(lines, boxWidth)
    val bottomLine: Line = renderBottomLine(boxWidth)

    topLine +: innerLines :+ bottomLine
  }

  private def renderTopLine(title: String, boxWidth: Int): Line = {
    val availableTitleWidth = math.max(0, boxWidth - NumberOfTopLineCharsNotUsedForTitle)
    val truncatedTitle = StringUtils.ellipsisise(title, availableTitleWidth)
    val surplusWidth = availableTitleWidth - truncatedTitle.length
    val filler = "─" * surplusWidth
    Line(TopLineStart.style + truncatedTitle.style(TitleStyle) + (" " + filler + TopLineEnd).style)
  }

  private def renderInnerLines(lines: Seq[StyledString], boxWidth: Int): Seq[Line] = {
    val availableInnerWidth = math.max(0, boxWidth - NumberOfInnerLineCharsNotUsedForContent)
    val truncatedLines = lines.map(StyledStringUtils.ellipsisise(_, availableInnerWidth))
    truncatedLines.map { content ⇒
      val surplusWidth = availableInnerWidth - content.length
      val filler = " " * surplusWidth
      Line(InnerLineStart.style + content + filler.style + InnerLineEnd.style)
    }
  }

  private def renderBottomLine(boxWidth: Int): Line = {
    val surplusWidth = boxWidth - NumberOfBottomLineCharsNotUsedForFiller
    val filler = "─" * surplusWidth
    Line((BottomLineStart + filler + BottomLineEnd).style)
  }

  private def getDesiredInnerLineWidth(line: StyledString): Int = line.length + InnerLineStart.length + InnerLineEnd.length

  private def getDesiredTopLineWidth(title: String): Int = title.length + NumberOfTopLineCharsNotUsedForTitle

}
