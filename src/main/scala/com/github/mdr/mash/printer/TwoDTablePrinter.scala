package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.printer.model.TwoDTableModelCreator
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }
import com.github.mdr.mash.terminal.TerminalInfo

class TwoDTablePrinter(output: PrintStream, terminalInfo: TerminalInfo, viewConfig: ViewConfig) {

  def printTable(value: MashValue) {
    val creator = new TwoDTableModelCreator(terminalInfo, showSelections = false, viewConfig)
    val model = creator.create(value)
    val stringifier = new TwoDTableStringifier(showSelections = false)
    output.println(stringifier.renderTopRow(model))
    output.println(stringifier.renderHeaderRow(model))
    output.println(stringifier.renderBelowHeaderRow(model))
    for (row ← model.rows)
      output.println(stringifier.renderObjectRow(model, row))
    output.println(stringifier.renderBottomRow(model))
  }

}