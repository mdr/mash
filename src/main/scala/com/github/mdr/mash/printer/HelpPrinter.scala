package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.render.help.{ ClassHelpRenderer, FieldHelpRenderer, FunctionHelpRenderer, HelpRenderer }
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.screen.{ Line, Screen }

class HelpPrinter(output: PrintStream) {

  def printMethodHelp(obj: MashObject) = printAll(FunctionHelpRenderer.renderMethod(obj))

  def printFunctionHelp(obj: MashObject) = printAll(FunctionHelpRenderer.render(obj))

  def printFieldHelp(obj: MashObject) = printAll(FieldHelpRenderer.render(obj))

  def printClassHelp(obj: MashObject) = printAll(ClassHelpRenderer.render(obj))

  private def printAll(lines: Seq[Line]): Unit =
    for (line ← lines)
      output.println(Screen.drawStyledChars(line.string))

}