package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.os.linux.LinuxProcessInteractions
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter

object ProcessesFunction extends MashFunction("os.processes") {

  private val processInteractions = LinuxProcessInteractions

  val params = ParameterModel()

  def apply(arguments: Arguments): Seq[MashObject] = {
    params.validate(arguments)
    processInteractions.getProcesses.map(ProcessClass.makeProcess)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Seq(Type.Instance(ProcessClass)))

  override def summary = "List processes"

}