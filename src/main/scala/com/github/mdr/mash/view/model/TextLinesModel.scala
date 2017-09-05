package com.github.mdr.mash.view.model

import com.github.mdr.mash.runtime.MashList

case class TextLinesModel(renderedLines: Seq[String], 
                          rawValue: MashList) extends DisplayModel
