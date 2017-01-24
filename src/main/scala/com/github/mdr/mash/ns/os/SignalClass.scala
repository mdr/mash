package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.ns.core.AnyClass
import jnr.constants.platform.linux.Signal

object SignalClass extends MashClass("os.Signal") {

  override def enumerationValues: Option[Seq[String]] = Some(Signals)

  val Signals = Signal.values.toSeq.map(_.name.drop(3))

  override def summaryOpt = Some("A signal")

  override def parentOpt = Some(AnyClass)

}