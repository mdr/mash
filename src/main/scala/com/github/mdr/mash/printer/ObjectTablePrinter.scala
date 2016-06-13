package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.terminal.TerminalInfo

class ObjectTablePrinter(output: PrintStream, terminalInfo: TerminalInfo) {

  def printTable(objects: Seq[MashObject]) {
    val renderer = new ObjectTableRenderer(terminalInfo, showSelections = false)
    val model = renderer.renderObjects(objects)
    output.println(renderer.renderTopRow(model))
    output.println(renderer.renderHeaderRow(model))
    output.println(renderer.renderBelowHeaderRow(model))
    for (obj ← model.objects)
      output.println(renderer.renderObjectRow(model, obj))
    output.println(renderer.renderBottomRow(model))
  }

}