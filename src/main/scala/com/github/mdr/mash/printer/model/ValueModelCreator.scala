package com.github.mdr.mash.printer.model

import com.github.mdr.mash.printer.{ FieldRenderer, ViewConfig }
import com.github.mdr.mash.runtime.MashValue

class ValueModelCreator(viewConfig: ViewConfig) {

  private val fieldRenderer = new FieldRenderer(viewConfig)

  def create(value: MashValue): ValueModel =
    ValueModel(fieldRenderer.renderField(value), value)
}
