package com.github.mdr.mash.printer.model

import com.github.mdr.mash.runtime.MashList

case class TextLinesModel(renderedLines: Seq[String], rawValue: MashList) extends DisplayModel
