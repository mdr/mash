package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.printer.model.ObjectsTableModelCreator
import com.github.mdr.mash.runtime.{ MashList, MashObject }
import com.github.mdr.mash.terminal.TerminalInfo

class ObjectsTablePrinter(output: PrintStream, terminalInfo: TerminalInfo, viewConfig: ViewConfig) {

  def printTable(objects: Seq[MashObject]) {
    val creator = new ObjectsTableModelCreator(terminalInfo, showSelections = false, viewConfig)
    val model = creator.create(objects, MashList(objects))
    val stringifier = new ObjectsTableStringifier(terminalInfo, showSelections = false)
    output.println(stringifier.renderTopRow(model))
    output.println(stringifier.renderHeaderRow(model))
    output.println(stringifier.renderBelowHeaderRow(model))
    for (obj ← model.objects)
      output.println(stringifier.renderObjectRow(model, obj))
    output.println(stringifier.renderBottomRow(model))
  }

}