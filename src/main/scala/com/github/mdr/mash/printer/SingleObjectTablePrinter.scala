package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.printer.model.SingleObjectTableModelCreator
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Screen
import com.github.mdr.mash.screen.browser.SingleObjectTableCommonRenderer
import com.github.mdr.mash.utils.Dimension

class SingleObjectTablePrinter(output: PrintStream, terminalSize: Dimension, viewConfig: ViewConfig) {

  def printObject(obj: MashObject) = {
    val model = new SingleObjectTableModelCreator(terminalSize, supportMarking = false, viewConfig).create(obj)
    val renderer = new SingleObjectTableCommonRenderer(model)
    val lines = renderer.renderTableLines()
    for (line ← lines)
      output.println(Screen.drawStyledChars(line.string))
  }

}