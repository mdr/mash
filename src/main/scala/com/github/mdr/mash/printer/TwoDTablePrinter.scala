package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.printer.model.TwoDTableModelCreator
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.screen.Screen
import com.github.mdr.mash.screen.browser.TwoDTableCommonRenderer
import com.github.mdr.mash.terminal.TerminalInfo

class TwoDTablePrinter(output: PrintStream, terminalInfo: TerminalInfo, viewConfig: ViewConfig) {

  def printTable(value: MashValue) = {
    val creator = new TwoDTableModelCreator(terminalInfo, showSelections = false, viewConfig)
    val model = creator.create(value)
    val lines = new TwoDTableCommonRenderer(model, showSelections = false).renderAllTableLines
    for (line ← lines)
      output.println(Screen.drawStyledChars(line.string))
  }

}