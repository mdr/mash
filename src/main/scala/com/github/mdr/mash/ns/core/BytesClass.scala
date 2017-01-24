package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.MashClass

/**
 * A units type: number of bytes
 */
object BytesClass extends MashClass("core.Bytes") {

  override def summaryOpt = Some("Tag class representing a quantity of bytes")

  override def parentOpt = Some(AnyClass)

}
