package com.github.mdr.mash.view.printer

import java.io.PrintStream

import com.github.mdr.mash.view.model.TwoDTableModelCreator
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.screen.{ Screen, StyledStringDrawer }
import com.github.mdr.mash.utils.Dimensions
import com.github.mdr.mash.view.ViewConfig
import com.github.mdr.mash.view.common.TwoDTableCommonRenderer

class TwoDTablePrinter(output: PrintStream, terminalSize: Dimensions, viewConfig: ViewConfig) {

  def printTable(value: MashValue) = {
    val creator = new TwoDTableModelCreator(terminalSize, supportMarking = false, viewConfig)
    val model = creator.create(value)
    val lines = new TwoDTableCommonRenderer(model, terminalSize).renderAllTableLines
    for (line ← lines)
      output.println(StyledStringDrawer.drawStyledChars(line.string))
  }

}