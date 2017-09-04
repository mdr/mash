package com.github.mdr.mash.view.render

import java.util.Date

import com.github.mdr.mash.evaluator.TildeExpander
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.view.render.KeyHint._
import com.github.mdr.mash.repl.IncrementalHistorySearchState
import com.github.mdr.mash.repl.IncrementalHistorySearchState.{ Hit, HitStatus }
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen._
import com.github.mdr.mash.utils.{ Dimensions, Point, StyledStringUtils }
import com.github.mdr.mash.view.Viewer

object IncrementalHistorySearchRenderer {

  private val envInteractions = LinuxEnvironmentInteractions

  def renderHistorySearchState(searchState: IncrementalHistorySearchState, terminalSize: Dimensions): LinesAndCursorPos = {
    val prefix = "Incremental history search: ".style
    val searchString = searchState.searchString.style(SearchStringStyle)
    val chars = prefix + searchString
    val searchLine = Line(chars)
    val cursorPosOpt = Some(Point(0, chars.size))

    val hintLine = renderHitLine(searchState.hitStatus)
    val lines = Seq(searchLine) ++ hintLine ++ Seq(keyHintLine(searchState))
    LinesAndCursorPos(lines, cursorPosOpt)
  }

  private def keyHintLine(searchState: IncrementalHistorySearchState) = {
    val hints = Seq(NextHistoryHit, DoneSearch, Quit, ChangeDirectory) :+ (if (searchState.currentDirOnly) AllDirs else ThisDirOnly)
    Line(KeyHint.renderKeyHints(hints))
  }

  private def renderHitLine(hitStatus: HitStatus): Option[Line] =
    hitStatus match {
      case Hit(resultIndex, _, timestamp, workingDirectory) ⇒
        val time = Viewer.prettyTime.format(Date.from(timestamp))
        val directoryWithTilde = new TildeExpander(envInteractions).retilde(workingDirectory.toString)
        Some(Line(("Hit " + (resultIndex + 1) + ": " + time + " in " + directoryWithTilde).style(HitStyle)))
      case _                                                ⇒
        None
    }

  private val HitStyle = Style(foregroundColour = BasicColour.Grey)

  private val SearchStringStyle = Style(foregroundColour = BasicColour.Cyan)

  private def truncateIfNecessary(line: Line, terminalSize: Dimensions): Line =
    Line(StyledStringUtils.ellipsisise(line.string, terminalSize.columns))

}
