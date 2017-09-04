package com.github.mdr.mash.printer

import java.io.PrintStream

import com.github.mdr.mash.printer.model.ObjectTreeModelCreator
import com.github.mdr.mash.render.browser.ObjectTreeCommonRenderer
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.screen.StyledStringDrawer
import com.github.mdr.mash.utils.Dimensions

class ObjectTreePrinter(output: PrintStream, terminalSize: Dimensions, viewConfig: ViewConfig) {

  def print(value: MashValue) = {
    val model = new ObjectTreeModelCreator(viewConfig).create(value)
    val renderer = new ObjectTreeCommonRenderer(model, selectionPathOpt = None, terminalSize)
    val lines = renderer.renderTableLines
    for (line ← lines)
      output.println(StyledStringDrawer.drawStyledChars(line.string))
  }

}