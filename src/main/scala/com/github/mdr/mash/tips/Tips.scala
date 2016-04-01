package com.github.mdr.mash.tips

import scala.util.Random
import java.io.PrintStream
import com.github.mdr.mash.ReplState
import com.github.mdr.mash.ns.os.OldDirsFunction
import com.github.mdr.mash.ns.os.UpFunction
import com.github.mdr.mash.ns.os.GlobFunction
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.printer.Printer

object Tips {

  val Tips = Seq(
    "Ctrl-Space provides context assistance when invoking a function/method.",
    s"To use unbound identifiers as strings: 'config.${ReplState.BareWordsConfigKey} = true'.",
    s"'${OldDirsFunction.name}' stores a list of previous working directories from this session.",
    s"'${UpFunction.name}' changes the current directory to the parent.",
    s"""'${GlobFunction.name}' can be used to match paths recursively: glob "**/*.jpg""",
    "Relative times are available on integers: 1.day.ago, 3.weeks.fromNow",
    "Comparisons can be chained: 0 <= i < 10",
    "Ctrl-r starts an incremental search through history.",
    "Ctrl-q quotes the current word.",
    "Alt-. inserts the last argument from history.",
    "If there are multiple completions, pressing tab twice enters completion browsing mode.",
    "Member accesses on lists can be automatically vectorised: ls.size.sum",
    """Strings can be interpolated: "Hello $user.name"""")

  private def randomTip = Tips(Random.nextInt(Tips.length))

  def showTip(output: PrintStream, terminalInfo: TerminalInfo) {
    val printer = new Printer(output, terminalInfo)
    val suppressNote = s"""Suppress tips by setting config.${ReplState.ShowStartupTipsConfigKey} = false."""
    printer.renderBox("Tip", Seq(randomTip, suppressNote))
  }
  
}