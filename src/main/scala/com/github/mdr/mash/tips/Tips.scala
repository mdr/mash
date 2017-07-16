package com.github.mdr.mash.tips

import java.io.PrintStream

import com.github.mdr.mash.Config
import com.github.mdr.mash.ns.os.{ GlobFunction, OldDirsFunction, UpFunction }
import com.github.mdr.mash.utils.{ Dimensions, StringUtils }

import scala.util.Random

object Tips {

  val Tips = Seq(
    "Ctrl-Space provides context assistance when invoking a function/method.",
    s"To use unbound identifiers as strings: 'config.${Config.Language.BareWords} = true'.",
    s"'${OldDirsFunction.name}' stores a list of previous working directories from this session.",
    s"'${UpFunction.name}' changes the current directory to the parent.",
    s"""'${GlobFunction.name}' can be used to match paths recursively: glob "**/*.jpg""",
    "Relative times are available on integers: 1.day.ago, 3.weeks.fromNow",
    "Comparisons can be chained: 0 <= i < 10",
    "Ctrl-r starts an incremental search through history.",
    "Ctrl-q toggles quotation marks around the current region of text.",
    "Alt-. inserts the last argument from history.",
    "Press tab twice to enter completion browsing mode.",
    "Member accesses on lists can be automatically vectorised: ls.size.sum",
    """Strings can be interpolated: "Hello $user.name"""",
    s"Suppress tips by setting config.${Config.Cli.ShowStartupTips} = false.",
    s"Set config.${Config.View.FuzzyTime} to view date/times as '3 hours ago', etc.",
    s"Set config.${Config.View.BrowseLargeOutput} = false to avoid starting the browser on long output")

  private def randomTip = Tips(Random.nextInt(Tips.length))

  def showTip(output: PrintStream, terminalSize: Dimensions) {
    printBox("Tip", Seq(randomTip), output, terminalSize)
  }

  private def printBox(title: String, lines: Seq[String], output: PrintStream, terminalSize: Dimensions) {
    val boxWidth = math.min(math.max(lines.map(_.size + 4).max, title.size + 4), terminalSize.columns)
    val innerWidth = boxWidth - 4
    val displayTitle = " " + StringUtils.ellipsisise(title, innerWidth) + " "
    val displayLines = lines.map(l ⇒ StringUtils.ellipsisise(l, innerWidth))
    val topLine = "┌─" + displayTitle + "─" * (innerWidth - displayTitle.length) + "─┐"
    val bottomLine = "└─" + "─" * innerWidth + "─┘"
    val contentLines = displayLines.map(l ⇒ "│ " + l + " " * (innerWidth - l.length) + " │")
    for (line ← topLine +: contentLines :+ bottomLine)
      output.println(line)
  }

}