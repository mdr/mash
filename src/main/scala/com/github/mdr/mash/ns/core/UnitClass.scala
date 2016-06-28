package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.MashClass

object UnitClass extends MashClass("core.Unit") {

  override def summary = "The type of the unit value"

  override def parentOpt = Some(AnyClass)

}