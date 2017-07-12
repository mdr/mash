package com.github.mdr.mash.printer.model

import com.github.mdr.mash.runtime.MashValue

case class ValueModel(renderedValue: String, rawValue: MashValue) extends DisplayModel
