package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.ns.git.StatusClass
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.BasicColour
import com.github.mdr.mash.screen.Screen._
import com.github.mdr.mash.screen.Style.StylableString

/**
 * Render a git.Status object in the style of the command-line "git status" output
 */
class GitStatusPrinter(output: PrintStream) {

  def print(obj: MashObject) {
    val status = StatusClass.Wrapper(obj)
    def red(s: String) = drawStyledChars(s.style(foregroundColour = BasicColour.Red))
    def green(s: String) = drawStyledChars(s.style(foregroundColour = BasicColour.Green))
    val indent = " " * 8
    output.println("On branch " + status.branch)
    val upstreamBranch = status.upstreamBranchOpt.getOrElse("")
    def commitWord(n: Int) = if (n == 1) "commit" else "commits"
    if (status.aheadCount > 0)
      if (status.behindCount > 0) {
        output.println(s"Your branch and '$upstreamBranch' have diverged,")
        output.println(s"and have ${status.aheadCount} and ${status.behindCount} different commits each, respectively.")
        output.println("""  (use "git.pull" to merge the remote branch into yours)""")
      } else {
        output.println(s"Your branch is ahead of '$upstreamBranch' by ${status.aheadCount} ${commitWord(status.aheadCount)}.")
        output.println("""  (use "git.push" to publish your local commits)""")
      }
    else if (status.behindCount > 0) {
      output.println(s"Your branch is behind '$upstreamBranch' by ${status.behindCount} ${commitWord(status.aheadCount)}, and can be fast-forwarded.")
      output.println("""  (use "git.pull" to update your local branch)""")
    }

    if (status.hasChangesToBeCommitted) {
      output.println("Changes to be committed:")
      output.println("""(use "git.unstage <file>..." to unstage)""")
      output.println()
      for (path ← status.added)
        output.println(green(indent + "new file:   " + path))
      for (path ← status.changed)
        output.println(green(indent + "modified:   " + path))
      for (path ← status.removed)
        output.println(green(indent + "deleted:    " + path))
      output.println()
    }
    if (status.hasUnstagedChanges) {
      output.println("Changed not staged for commit:")
      output.println("""  (use "git.stage <file>..." to update what will be committed)""")
      output.println("""  (use "git.restore <file>..." to discard changes in working directory)""")
      output.println()
      for (path ← status.modified)
        output.println(red(indent + "modified:   " + path))
      for (path ← status.missing)
        output.println(red(indent + "deleted:    " + path))
      output.println()
    }
    if (status.conflicting.nonEmpty) {
      output.println("Unmerged paths:")
      output.println("""  (use "git.stage <file>..." to mark resolution)""")
      output.println()
      for (path ← status.conflicting)
        output.println(red(indent + "both modified:   " + path))
      output.println()
    }
    if (status.untracked.nonEmpty) {
      output.println("Untracked files:")
      output.println("""  (use "git.stage <file>..." to include in what will be committed)""")
      output.println()
      for (path ← status.untracked)
        output.println(red(indent + path))
      output.println()
    }
    if (!status.hasChangesToBeCommitted && !status.hasUnstagedChanges && status.untracked.isEmpty)
      output.println("nothing to commit, working directory clean")
  }

}