package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.terminal.TerminalInfo

class ObjectTablePrinter(output: PrintStream, terminalInfo: TerminalInfo) {

  def printTable(objects: Seq[MashObject]) {
    val creator = new ObjectTableModelCreator(terminalInfo, showSelections = false)
    val model = creator.create(objects)
    val stringifier = new ObjectTableStringifier(terminalInfo, showSelections = false)
    output.println(stringifier.renderTopRow(model))
    output.println(stringifier.renderHeaderRow(model))
    output.println(stringifier.renderBelowHeaderRow(model))
    for (obj ← model.objects)
      output.println(stringifier.renderObjectRow(model, obj))
    output.println(stringifier.renderBottomRow(model))
  }

}