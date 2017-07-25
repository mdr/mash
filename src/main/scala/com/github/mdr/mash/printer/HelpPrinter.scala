package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.render.HelpRenderer
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.screen.{ Line, Screen }

class HelpPrinter(output: PrintStream) {

  def printFunctionHelp(obj: MashObject) = printAll(HelpRenderer.renderFunctionHelp(obj))

  def printFieldHelp(obj: MashObject) = printAll(HelpRenderer.renderFieldHelp(obj))

  def printClassHelp(obj: MashObject) = printAll(HelpRenderer.renderClassHelp(obj))

  private def printAll(lines: Seq[Line]): Unit =
    for (line ← lines)
      output.println(Screen.drawStyledChars(line.string))

}