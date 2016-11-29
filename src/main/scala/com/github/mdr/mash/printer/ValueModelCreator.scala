package com.github.mdr.mash.printer

import com.github.mdr.mash.runtime.{ MashList, MashValue }
import com.github.mdr.mash.terminal.TerminalInfo

class ValueModelCreator(terminalInfo: TerminalInfo, viewConfig: ViewConfig) {

  private val fieldRenderer = new FieldRenderer(viewConfig)

  def create(value: MashValue): ValueModel =
    ValueModel(fieldRenderer.renderField(value), value)
}
