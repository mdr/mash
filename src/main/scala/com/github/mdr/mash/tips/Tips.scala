package com.github.mdr.mash.tips

import java.io.PrintStream

import com.github.mdr.mash.Config
import com.github.mdr.mash.ns.os.{ GlobFunction, OldDirsFunction, UpFunction }
import com.github.mdr.mash.render.help.MashMarkupRenderer
import com.github.mdr.mash.render.{ BoxContent, BoxRenderer }
import com.github.mdr.mash.screen.{ Screen, StyledString, StyledStringDrawer }
import com.github.mdr.mash.utils.Dimensions

import scala.util.Random

object Tips {

  val Tips = Seq(
    "Ctrl-Space provides context assistance when invoking a function/method.",
    s"To use unbound identifiers as strings: <mash>config.${Config.Language.BareWords} = true</mash>",
    s"<mash>${OldDirsFunction.name}</mash> stores a list of previous working directories from this session.",
    s"<mash>${UpFunction.name}</mash> changes the current directory to the parent.",
    s"""<mash>${GlobFunction.name}</mash> can be used to match paths recursively: <mash>glob "**/*.jpg</mash>""",
    "Relative times are available on integers: <mash>1.day.ago</mash>, <mash>3.weeks.fromNow</mash>",
    "Comparisons can be chained: <mash>0 <= i < 10</mash>",
    "Ctrl-r starts an incremental search through history.",
    "Ctrl-q toggles quotation marks around the current region of text.",
    "Alt-. inserts the last argument from history.",
    "Alt-e evaluates the current command (or selection), and replaces the line buffer (or selection) with the result.",
    "Press tab twice to enter completion browsing mode.",
    "Member accesses on lists can be automatically vectorised: <mash>ls.size.sum</mash>",
    """Strings can be interpolated: <mash>"Hello $user.name"</mash>""",
    s"Suppress tips by setting <mash>config.${Config.Cli.ShowStartupTips} = false</mash>.",
    s"Set <mash>config.${Config.View.FuzzyTime} = true</mash> to view date/times as '3 hours ago', etc.",
    s"Set <mash>config.${Config.View.BrowseLargeOutput} = false</mash> to avoid starting the browser on long output.",
    "Alt-Up expands the current selection to the nearest enclosing piece of syntax; Alt-Down unexpands.",
    "Shift-left/right/up/down expands the selection.",
    "Ctrl-_ undoes the previous edit",
    "Ctrl-w cuts the current selection, Alt-w copies, and Ctrl-y pastes.")

  private def randomTip = Tips(Random.nextInt(Tips.length))

  def showTip(output: PrintStream, terminalSize: Dimensions) {
    val tipLine: StyledString = MashMarkupRenderer.render(randomTip)
    val boxContent: BoxContent = BoxContent(title = "Tip", lines = Seq(tipLine))
    val lines = BoxRenderer.render(boxContent, terminalSize)
    for (line ← lines)
      output.println(StyledStringDrawer.drawStyledChars(line.string))
  }

}