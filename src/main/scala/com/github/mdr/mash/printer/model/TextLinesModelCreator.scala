package com.github.mdr.mash.printer.model

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.printer.ViewConfig
import com.github.mdr.mash.runtime.MashList

class TextLinesModelCreator(viewConfig: ViewConfig) {

  def create(list: MashList): TextLinesModel = {
    val lines =
      for (line <- list.items)
        yield ToStringifier.stringify(line)
    TextLinesModel(lines, list)
  }

}
