package com.github.mdr.mash.printer.model

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.printer.ViewConfig
import com.github.mdr.mash.render.help._
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.utils.Dimensions

class HelpModelCreator(terminalSize: Dimensions,
                       viewConfig: ViewConfig) {

  def createForMethod(obj: MashObject): HelpModel = {
    val LinesAndLinks(lines, links) = MethodHelpRenderer.render(obj)
    HelpModel(obj, lines.map(_.string), links)
  }

  def createForField(obj: MashObject): HelpModel = {
    val LinesAndLinks(lines, links) = FieldHelpRenderer.render(obj)
    HelpModel(obj, lines.map(_.string), links)
  }

  def createForFunction(f: MashFunction): HelpModel = {
    val LinesAndLinks(lines, links) = FunctionHelpRenderer.render(f)
    HelpModel(f, lines.map(_.string), links)
  }

  def createForClass(klass: MashClass): HelpModel = {
    val LinesAndLinks(lines, links) = ClassHelpRenderer.render(klass)
    HelpModel(klass, lines.map(_.string), links)
  }

}
