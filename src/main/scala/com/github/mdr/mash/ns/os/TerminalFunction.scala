package com.github.mdr.mash.ns.os

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.runtime.{ MashNumber, MashObject }
import com.github.mdr.mash.utils.Dimensions

import scala.collection.immutable.ListMap

object TerminalFunction extends MashFunction("os.terminal") {

  val params = ParameterModel.Empty

  def call(boundParams: BoundParams): MashObject = {
    val Dimensions(rows, columns) = Singletons.terminal.size
    import TerminalClass.Fields._
    MashObject.of(
      ListMap(
        Rows → MashNumber(rows),
        Columns → MashNumber(columns)),
      TerminalClass)
  }

  override def typeInferenceStrategy = TerminalClass

  override def summaryOpt = Some("Return information about the current terminal")

}
