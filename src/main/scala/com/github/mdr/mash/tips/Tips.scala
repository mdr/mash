package com.github.mdr.mash.tips

import java.io.PrintStream

import com.github.mdr.mash.Config
import com.github.mdr.mash.ns.os.{ GlobFunction, OldDirsFunction, UpFunction }
import com.github.mdr.mash.printer.Printer
import com.github.mdr.mash.terminal.TerminalInfo

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
    s"Set config.${Config.View.FuzzyTime} to view date/times as '3 hours ago', etc.")

  private def randomTip = Tips(Random.nextInt(Tips.length))

  def showTip(output: PrintStream, terminalInfo: TerminalInfo) {
    val printer = new Printer(output, terminalInfo)
    printer.printBox("Tip", Seq(randomTip))
  }

}