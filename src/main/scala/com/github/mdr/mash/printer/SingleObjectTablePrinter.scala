package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.printer.model.SingleObjectTableModelCreator
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Screen
import com.github.mdr.mash.screen.browser.SingleObjectTableCommonRenderer
import com.github.mdr.mash.terminal.TerminalInfo

class SingleObjectTablePrinter(output: PrintStream, terminalInfo: TerminalInfo, viewConfig: ViewConfig) {

  def printObject(obj: MashObject) = {
    val model = new SingleObjectTableModelCreator(terminalInfo, viewConfig).create(obj)
    val commonRenderer = new SingleObjectTableCommonRenderer(model, terminalInfo)
    val lines = commonRenderer.renderTableLines(model.fields.toSeq)
    for (line ← lines)
      output.println(Screen.drawStyledChars(line.string))
  }

}