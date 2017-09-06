package com.github.mdr.mash.view.printer

import java.io.PrintStream

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.screen.{ Line, StyledStringDrawer }
import com.github.mdr.mash.view.render.help.{ ClassHelpRenderer, FieldHelpRenderer, FunctionHelpRenderer, MethodHelpRenderer }

class HelpPrinter(output: PrintStream) {

  def printMethodHelp(obj: MashObject) = printAll(MethodHelpRenderer.render(obj).lines)

  def printFunctionHelp(f: MashFunction) = printAll(FunctionHelpRenderer.render(f).lines)

  def printFieldHelp(obj: MashObject) = printAll(FieldHelpRenderer.render(obj).lines)

  def printClassHelp(klass: MashClass) = printAll(ClassHelpRenderer.render(klass).lines)

  private def printAll(lines: Seq[Line]): Unit =
    for (line ← lines)
      output.println(StyledStringDrawer.drawStyledChars(line.string))

}