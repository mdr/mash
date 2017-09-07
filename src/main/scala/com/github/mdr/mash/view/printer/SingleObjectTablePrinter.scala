package com.github.mdr.mash.view.printer

import java.io.PrintStream

import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.StyledStringDrawer
import com.github.mdr.mash.utils.Dimensions
import com.github.mdr.mash.utils.Utils._
import com.github.mdr.mash.view.ViewConfig
import com.github.mdr.mash.view.common.SingleObjectTableCommonRenderer
import com.github.mdr.mash.view.model.SingleObjectTableModelCreator
import com.github.mdr.mash.view.render.DiscoBorders

class SingleObjectTablePrinter(output: PrintStream, terminalSize: Dimensions, viewConfig: ViewConfig) {

  def printObject(obj: MashObject) {
    val model = new SingleObjectTableModelCreator(terminalSize, supportMarking = false, viewConfig).create(obj)
    val renderer = new SingleObjectTableCommonRenderer(model)
    val lines = renderer.renderTableLines().whenOpt(viewConfig.discoModeOpt)(DiscoBorders.addDiscoBorders)
    for (line ← lines)
      output.println(StyledStringDrawer.drawStyledChars(line.string))
  }

}