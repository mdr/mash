package com.github.mdr.mash.printer

import java.io.PrintStream
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.ns.git.StatusClass
import org.fusesource.jansi.Ansi
import org.fusesource.jansi.Ansi.Color

class GitStatusPrinter(output: PrintStream) {

  def print(mo: MashObject) {
    val status = StatusClass.Wrapper(mo)
    def red(s: String) = Ansi.ansi().fg(Color.RED).a(s).reset
    def green(s: String) = Ansi.ansi().fg(Color.GREEN).a(s).reset
    val indent = " " * 8
    if (status.hasChangesToBeCommitted) {
      output.println("Changes to be committed:")
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
      output.println("""  (use "git.add <file>..." to update what will be committed)""")
      output.println()
      for (path ← status.modified)
        output.println(red(indent + "modified:   " + path))
      for (path ← status.missing)
        output.println(red(indent + "deleted:    " + path))
      output.println()
    }
    if (status.untracked.nonEmpty) {
      output.println("Untracked files:")
      output.println("""  (use "git.add <file>..." to include in what will be committed)""")
      output.println()
      for (path ← status.untracked)
        output.println(red(indent + path))
      output.println
    }

  }

}