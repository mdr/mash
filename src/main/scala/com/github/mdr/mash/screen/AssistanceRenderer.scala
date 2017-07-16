package com.github.mdr.mash.screen

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.utils.Dimensions
import com.github.mdr.mash.utils.StringUtils.ellipsisise

object AssistanceRenderer {

  def render(assistanceState: AssistanceState, terminalSize: Dimensions): Seq[Line] = {
    val title = assistanceState.title
    val lines = assistanceState.lines
    val boxWidth = math.min(math.max(lines.map(_.size + 4).max, title.size + 6), terminalSize.columns)
    val innerWidth = boxWidth - 4

    val displayTitle = ellipsisise(title, innerWidth)
    val styledTitle = displayTitle.style(TitleStyle)
    val topLine = Line("┌─ ".style + styledTitle + (" " + "─" * (innerWidth - displayTitle.length - 2) + "─┐").style)

    val displayLines = lines.map(ellipsisise(_, innerWidth))
    val contentLines = displayLines.map(l ⇒ Line(("│ " + l + " " * (innerWidth - l.length) + " │").style))

    val bottomLine = Line(("└─" + "─" * innerWidth + "─┘").style)

    topLine +: contentLines :+ bottomLine
  }

  private val TitleStyle = Style(bold = true, foregroundColour = BasicColour.Yellow)

}
