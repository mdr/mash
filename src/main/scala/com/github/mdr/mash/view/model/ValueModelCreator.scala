package com.github.mdr.mash.view.model

import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.view.{ FieldRenderer, ViewConfig }

class ValueModelCreator(viewConfig: ViewConfig) {

  private val fieldRenderer = new FieldRenderer(viewConfig)

  def create(value: MashValue): ValueModel =
    ValueModel(fieldRenderer.renderField(value), value)
}
