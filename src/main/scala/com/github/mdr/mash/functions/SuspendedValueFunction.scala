package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.{ Arguments, SuspendedMashValue }
import com.github.mdr.mash.runtime.MashValue

case class SuspendedValueFunction(suspendedValue: SuspendedMashValue) extends MashFunction(nameOpt = None) {

  val params = ParameterModel()

  def apply(arguments: Arguments): MashValue = {
    params.validate(arguments)
    suspendedValue.resolve()
  }

  override def summary = s"Lazily computed argument"

}
