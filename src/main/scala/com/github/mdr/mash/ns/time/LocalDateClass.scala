package com.github.mdr.mash.ns.time

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.ns.core.AnyClass

object LocalDateClass extends MashClass("time.Date") {

  override def summary = "A date"

  override def parentOpt = Some(AnyClass)

}