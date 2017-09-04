package com.github.mdr.mash.view.printer

import java.io.PrintStream

import com.github.mdr.mash.view.model.ObjectTreeModelCreator
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.screen.StyledStringDrawer
import com.github.mdr.mash.utils.Dimensions
import com.github.mdr.mash.view.ViewConfig
import com.github.mdr.mash.view.common.ObjectTreeCommonRenderer

class ObjectTreePrinter(output: PrintStream, terminalSize: Dimensions, viewConfig: ViewConfig) {

  def print(value: MashValue) = {
    val model = new ObjectTreeModelCreator(viewConfig).create(value)
    val renderer = new ObjectTreeCommonRenderer(model, selectionPathOpt = None, terminalSize)
    val lines = renderer.renderTableLines
    for (line ← lines)
      output.println(StyledStringDrawer.drawStyledChars(line.string))
  }

}