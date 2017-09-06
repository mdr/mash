package com.github.mdr.mash.view.printer

import java.io.PrintStream

import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.screen.StyledStringDrawer
import com.github.mdr.mash.utils.Dimensions
import com.github.mdr.mash.utils.Utils._
import com.github.mdr.mash.view.ViewConfig
import com.github.mdr.mash.view.common.TwoDTableCommonRenderer
import com.github.mdr.mash.view.model.TwoDTableModelCreator
import com.github.mdr.mash.view.render.DiscoBorders

class TwoDTablePrinter(output: PrintStream, terminalSize: Dimensions, viewConfig: ViewConfig) {

  def printTable(value: MashValue) {
    val creator = new TwoDTableModelCreator(terminalSize, supportMarking = false, viewConfig)
    val model = creator.create(value)
    val renderer = new TwoDTableCommonRenderer(model, terminalSize)
    val lines = renderer.renderAllTableLines.when(viewConfig.discoBorders, DiscoBorders.addDiscoBorders)
    
    for (line ← lines)
      output.println(StyledStringDrawer.drawStyledChars(line.string))
  }

}