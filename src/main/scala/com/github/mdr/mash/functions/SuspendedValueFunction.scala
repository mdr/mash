package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.{ Arguments, SuspendedMashValue }
import com.github.mdr.mash.runtime.MashValue

case class SuspendedValueFunction(suspendedValue: SuspendedMashValue) extends MashFunction(nameOpt = None) {

  val params = ParameterModel()

  def apply(boundParams: BoundParams): MashValue = suspendedValue.resolve()

  override def summaryOpt = Some("Lazily computed argument")

}
