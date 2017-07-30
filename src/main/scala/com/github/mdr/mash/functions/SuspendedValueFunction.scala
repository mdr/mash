package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.SuspendedMashValue
import com.github.mdr.mash.runtime.MashValue

case class SuspendedValueFunction(suspendedValue: SuspendedMashValue) extends MashFunction(nameOpt = None) {

  val params = ParameterModel.Empty

  def call(boundParams: BoundParams): MashValue = suspendedValue.resolve()

  override def summaryOpt = Some("Lazily computed argument")

}
