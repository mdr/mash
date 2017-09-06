package com.github.mdr.mash.view.printer

import java.io.PrintStream

import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.utils.Dimensions
import com.github.mdr.mash.utils.Utils._
import com.github.mdr.mash.view.ViewConfig
import com.github.mdr.mash.view.common.ObjectTreeCommonRenderer
import com.github.mdr.mash.view.model.{ ObjectTreeModel, ObjectTreeModelCreator }
import com.github.mdr.mash.view.render.DiscoBorders

class ObjectTreePrinter(output: PrintStream, terminalSize: Dimensions, viewConfig: ViewConfig) {
  
  def renderLines(value: MashValue): (Seq[Line], ObjectTreeModel) = {
    val model = new ObjectTreeModelCreator(viewConfig).create(value)
    val renderer = new ObjectTreeCommonRenderer(model, selectionPathOpt = None, terminalSize)
    val lines = renderer.renderTableLines.when(viewConfig.discoBorders, DiscoBorders.addDiscoBorders)
    (lines, model)
  }
  
}