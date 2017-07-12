package com.github.mdr.mash.printer.model

import com.github.mdr.mash.printer.ViewConfig
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }
import com.github.mdr.mash.printer.model.TwoDTableModelCreator.isSuitableForTwoDTable
import com.github.mdr.mash.terminal.TerminalInfo

trait DisplayModel

object DisplayModel {

  def getDisplayModel(value: MashValue, viewConfig: ViewConfig, terminalInfo: TerminalInfo): DisplayModel =
    value match {
      case _ if isSuitableForTwoDTable(value) ⇒
        new TwoDTableModelCreator(terminalInfo, supportMarking = true, viewConfig).create(value)
      case obj: MashObject if obj.nonEmpty    ⇒
        new SingleObjectTableModelCreator(terminalInfo, supportMarking = true, viewConfig).create(obj)
      case xs: MashList                       ⇒
        new TextLinesModelCreator(viewConfig).create(xs)
      case _                                  ⇒
        new ValueModelCreator(terminalInfo, viewConfig).create(value)
    }

}