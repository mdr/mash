package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator.MashClass
import jnr.constants.platform.linux.Signal

object SignalClass extends MashClass("os.Signal") {

  override def enumerationValues: Option[Seq[String]] = {
    val signals = Signal.values.toSeq.map(_.name.drop(3))
    Some(signals)
  }

    override def summary = "A signal"

}