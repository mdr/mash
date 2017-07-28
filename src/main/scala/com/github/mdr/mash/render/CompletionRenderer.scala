package com.github.mdr.mash.render

import com.github.mdr.mash.completions.{ Completion, CompletionFragment, CompletionType }
import com.github.mdr.mash.printer.Printer
import com.github.mdr.mash.repl.completions.{ BrowserCompletionState, CompletionState, IncrementalCompletionState }
import com.github.mdr.mash.screen._
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.utils.Utils._
import com.github.mdr.mash.utils.{ Dimensions, StringUtils }

import scala.PartialFunction._

case class CompletionRenderResult(lines: Seq[Line])

object CompletionRenderer {

  def renderCompletions(completionStateOpt: Option[CompletionState], availableSpace: Dimensions): CompletionRenderResult =
    completionStateOpt.map { completionState ⇒
      val completionDescriptionLines = renderCompletionDescription(completionState, availableSpace)
      val remainingRows = math.max(0, availableSpace.rows - completionDescriptionLines.size)
      val completionLines = renderCompletionOptions(completionState, availableSpace.copy(rows = remainingRows))
      val lines = completionLines ++ completionDescriptionLines
      CompletionRenderResult(lines)
    }.getOrElse(CompletionRenderResult(Seq()))

  private def getCompletionColour(completionTypeOpt: Option[CompletionType]): Colour =
    completionTypeOpt.collect {
      case CompletionType.Directory ⇒ BasicColour.Magenta
      case CompletionType.File      ⇒ BasicColour.Yellow
      case CompletionType.Flag      ⇒ BasicColour.Cyan
      case CompletionType.Binding   ⇒ DefaultColour
      case CompletionType.Function  ⇒ BasicColour.Green
      case CompletionType.Class     ⇒ BasicColour.Cyan
      case CompletionType.Field     ⇒ DefaultColour
      case CompletionType.Method    ⇒ BasicColour.Green
    }.getOrElse(BasicColour.Blue)

  def getNumberOfCompletionColumns(completionState: CompletionState, terminalSize: Dimensions): Int = {
    val completions = completionState.completions
    val terminalWidth = math.max(0, terminalSize.columns)
    val longestCompletionLength = completions.map(_.displayText.length).max
    val columnGap = " " * 2
    math.min(completions.size, math.max(1, (terminalWidth + columnGap.length) / (longestCompletionLength + columnGap.length)))
  }

  private def renderCompletionOptions(completionState: CompletionState, availableSpace: Dimensions): Seq[Line] = {
    val completions = completionState.completions
    val completionFragment = completionState match {
      case bcs: BrowserCompletionState     ⇒ CompletionFragment("", "", "")
      case ics: IncrementalCompletionState ⇒ ics.getCommonDisplayFragment
    }
    val terminalWidth = math.max(0, availableSpace.columns)
    val longestCompletionLength = completions.map(_.displayText.length).max
    val columnGap = " " * 2
    val numberOfCompletionColumns = getNumberOfCompletionColumns(completionState, availableSpace)
    val columnWidth = math.min(terminalWidth, longestCompletionLength)

    def renderCompletion(completion: Completion, index: Int): StyledString = {
      val Completion(displayText, _, _, completionTypeOpt, _, location) = completion
      val truncatedText = StringUtils.ellipsisise(displayText, columnWidth)
      val active = cond(completionState) { case bcs: BrowserCompletionState ⇒ bcs.activeCompletion == index }
      val colour = getCompletionColour(completionTypeOpt)

      val commonFragmentStyle = Style(foregroundColour = colour, bold = true, inverse = active)
      val commonFragmentStyled = completionFragment.text.style(commonFragmentStyle)

      val commonPrefix = completionFragment.prefix
      val commonPrefixStyled = commonPrefix.style(commonFragmentStyle)

      val beforeAfterStyle = Style(foregroundColour = colour, inverse = active)
      val before = truncatedText.window(completionFragment.prefix.length, location.displayPos - completionFragment.before.length - completionFragment.prefix.length)
      val beforeStyled = before.style(beforeAfterStyle)
      val after = truncatedText.drop(location.displayPos + completionFragment.after.length)
      val afterStyled = after.style(beforeAfterStyle)

      val padding = " " * (columnWidth - truncatedText.length)
      val paddingStyle = Style(foregroundColour = colour, inverse = active)
      val paddingStyled = padding.style(paddingStyle)

      commonPrefixStyled + beforeStyled + commonFragmentStyled + afterStyled + paddingStyled
    }

    val allLines =
      for {
        completionRow ← completions.zipWithIndex.grouped(numberOfCompletionColumns).toSeq
        styledChars = completionRow.map { case (completion, index) ⇒ renderCompletion(completion, index) }
        charsWithGaps = StyledString.join(styledChars, columnGap.style)
      } yield Line(charsWithGaps, endsInNewline = true)

    val activeIndex = condOpt(completionState) { case bcs: BrowserCompletionState ⇒ bcs.activeCompletion }.getOrElse(0)
    val activeRow = activeIndex / numberOfCompletionColumns
    val firstRow = math.max(0, activeRow - availableSpace.rows + 1)
    allLines.window(firstRow, availableSpace.rows)
  }

  private def renderCompletionDescription(completionState: CompletionState, terminalSize: Dimensions): Seq[Line] =
    completionState match {
      case bcs: BrowserCompletionState ⇒
        val activeCompletion = bcs.completions(bcs.activeCompletion)
        val description = activeCompletion.descriptionOpt.getOrElse("")
        val title =
          activeCompletion.typeOpt match {
            case Some(completionType) ⇒ s" ${completionType.name} "
            case None                 ⇒ ""
          }
        val boxWidth = math.min(math.max(description.size + 4, title.size + 4), terminalSize.columns)
        val innerWidth = boxWidth - 4
        val displayTitle = StringUtils.ellipsisise(title, innerWidth)
        val displayDescription = StringUtils.ellipsisise(Printer.replaceProblematicChars(description), innerWidth)
        Seq(
          Line(("┌─" + displayTitle + "─" * (innerWidth - displayTitle.length) + "─┐").style),
          Line(("│ " + displayDescription + " " * (innerWidth - displayDescription.length) + " │").style),
          Line(("└─" + "─" * innerWidth + "─┘").style))
      case _                           ⇒ Seq()
    }

}