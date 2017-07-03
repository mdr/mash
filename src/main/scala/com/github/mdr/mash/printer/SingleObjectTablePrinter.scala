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
    val renderer = new SingleObjectTableCommonRenderer(model)
    val lines = renderer.renderAllTableLines
    for (line ← lines)
      output.println(Screen.drawStyledChars(line.string))
  }

}