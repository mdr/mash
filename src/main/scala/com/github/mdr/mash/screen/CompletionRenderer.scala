package com.github.mdr.mash.screen

import scala.PartialFunction._
import org.fusesource.jansi.Ansi._
import org.fusesource.jansi.Ansi.Color._
import com.github.mdr.mash.repl.BrowserCompletionState
import com.github.mdr.mash.repl.CompletionState
import com.github.mdr.mash.repl.IncrementalCompletionState
import com.github.mdr.mash.completions.Completion
import com.github.mdr.mash.completions.CompletionType
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils
import com.github.mdr.mash.printer.Printer
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.completions.CompletionFragment

case class CompletionRenderResult(lines: Seq[Line], numberOfCompletionColumns: Int)

object CompletionRenderer {

  def renderCompletions(completionStateOpt: Option[CompletionState], terminalInfo: TerminalInfo): CompletionRenderResult =
    completionStateOpt.map { completionState ⇒
      val completionDescriptionLines = renderCompletionDescription(completionState, terminalInfo)
      val remainingRows = math.max(0, terminalInfo.rows - completionDescriptionLines.size)
      val (completionLines, numberOfCompletionColumns) = renderCompletionOptions(completionState, terminalInfo.copy(rows = remainingRows))
      val lines = completionLines ++ completionDescriptionLines
      CompletionRenderResult(lines, numberOfCompletionColumns)
    }.getOrElse(CompletionRenderResult(Seq(), 0))

  private def getCompletionColour(completionTypeOpt: Option[CompletionType]): Colour =
    completionTypeOpt.map {
      case CompletionType.Directory ⇒ Colour.Magenta
      case CompletionType.File      ⇒ Colour.Yellow
      case CompletionType.Flag      ⇒ Colour.Cyan
      case CompletionType.Binding   ⇒ Colour.Default
      case CompletionType.Function  ⇒ Colour.Green
      case CompletionType.Field     ⇒ Colour.Default
      case CompletionType.Method    ⇒ Colour.Green
    }.getOrElse(Colour.Blue)

  /**
   * Return the lines for the completion, and the number of completion columns
   */
  private def renderCompletionOptions(completionState: CompletionState, terminalInfo: TerminalInfo): (Seq[Line], Int) = {
    val completions = completionState.completions
    val completionFragment = completionState match {
      case bcs: BrowserCompletionState     ⇒ CompletionFragment("", "")
      case ics: IncrementalCompletionState ⇒ ics.getCommonDisplayText
    }
    val terminalWidth = math.max(0, terminalInfo.columns)
    val longestCompletionLength = completions.map(_.displayText.length).max
    val columnGap = " " * 2
    val numberOfCompletionColumns = math.min(completions.size, math.max(1, (terminalWidth + columnGap.length) / (longestCompletionLength + columnGap.length)))
    val columnWidth = math.min(terminalWidth, longestCompletionLength)

    def renderCompletion(completion: Completion, index: Int): Seq[StyledCharacter] = {
      val Completion(displayText, _, _, completionTypeOpt, _, location) = completion
      val truncatedText = StringUtils.ellipsisise(displayText, columnWidth)
      val active = cond(completionState) { case bcs: BrowserCompletionState ⇒ bcs.activeCompletion == index }
      val colour = getCompletionColour(completionTypeOpt)

      val commonFragmentStyle = Style(foregroundColour = colour, bold = true, inverse = active)
      val commonFragmentStyled = completionFragment.text.map(StyledCharacter(_, commonFragmentStyle))

      val beforeAfterStyle = Style(foregroundColour = colour, inverse = active)
      val before = truncatedText.take(location.displayPos - completionFragment.before.length)
      val beforeStyled = before.map(StyledCharacter(_, beforeAfterStyle))
      val after = truncatedText.drop(location.displayPos + completionFragment.after.length)
      val afterStyled = after.map(StyledCharacter(_, beforeAfterStyle))

      val padding = " " * (columnWidth - truncatedText.length)
      val paddingStyle = Style(foregroundColour = colour, inverse = active)
      val paddingStyled = padding.map(StyledCharacter(_, paddingStyle))

      beforeStyled ++ commonFragmentStyled ++ afterStyled ++ paddingStyled
    }

    val allLines =
      for {
        completionRow ← completions.zipWithIndex.grouped(numberOfCompletionColumns).toSeq
        styledChars = completionRow.map { case (completion, index) ⇒ renderCompletion(completion, index) }
        charsWithGaps = Utils.intercalate(styledChars, columnGap.map(StyledCharacter(_)))
      } yield Line(charsWithGaps, endsInNewline = true)

    val activeIndex = condOpt(completionState) { case bcs: BrowserCompletionState ⇒ bcs.activeCompletion }.getOrElse(0)
    val activeRow = activeIndex / numberOfCompletionColumns
    val firstRow = math.max(0, activeRow - terminalInfo.rows + 1)
    val truncatedLines = allLines.drop(firstRow).take(terminalInfo.rows)
    (truncatedLines, numberOfCompletionColumns)
  }

  private def renderCompletionDescription(completionState: CompletionState, terminalInfo: TerminalInfo): Seq[Line] =
    completionState match {
      case bcs: BrowserCompletionState ⇒
        val activeCompletion = bcs.completions(bcs.activeCompletion)
        activeCompletion.descriptionOpt.toSeq.flatMap { description ⇒
          val title =
            activeCompletion.typeOpt match {
              case Some(completionType) ⇒ s" ${completionType.name} "
              case None                 ⇒ ""
            }
          val boxWidth = math.min(math.max(description.size + 4, title.size + 4), terminalInfo.columns)
          val innerWidth = boxWidth - 4
          val displayTitle = StringUtils.ellipsisise(title, innerWidth)
          val displayDescription = StringUtils.ellipsisise(Printer.replaceProblematicChars(description), innerWidth)
          Seq(
            Line(("┌─" + displayTitle + "─" * (innerWidth - displayTitle.length) + "─┐").map(StyledCharacter(_))),
            Line(("│ " + displayDescription + " " * (innerWidth - displayDescription.length) + " │").map(StyledCharacter(_))),
            Line(("└─" + "─" * innerWidth + "─┘").map(StyledCharacter(_))))
        }
      case _ ⇒ Seq()
    }

}