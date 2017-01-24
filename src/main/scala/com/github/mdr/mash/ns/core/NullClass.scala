package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.MashClass

object NullClass extends MashClass("core.Null") {

  override def summaryOpt = Some("The type of null")

  override def parentOpt = Some(AnyClass)

}