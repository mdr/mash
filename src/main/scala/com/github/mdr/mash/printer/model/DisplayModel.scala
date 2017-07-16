package com.github.mdr.mash.printer.model

import com.github.mdr.mash.printer.ViewConfig
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }
import com.github.mdr.mash.printer.model.TwoDTableModelCreator.isSuitableForTwoDTable
import com.github.mdr.mash.utils.Dimensions

trait DisplayModel

object DisplayModel {

  def getDisplayModel(value: MashValue, viewConfig: ViewConfig, terminalSize: Dimensions): DisplayModel =
    value match {
      case _ if isSuitableForTwoDTable(value) ⇒
        new TwoDTableModelCreator(terminalSize, supportMarking = true, viewConfig).create(value)
      case obj: MashObject if obj.nonEmpty    ⇒
        new SingleObjectTableModelCreator(terminalSize, supportMarking = true, viewConfig).create(obj)
      case xs: MashList                       ⇒
        new TextLinesModelCreator(viewConfig).create(xs)
      case _                                  ⇒
        new ValueModelCreator(viewConfig).create(value)
    }

}