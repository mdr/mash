package com.github.mdr.mash.ns.os

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.os.linux.LinuxProcessInteractions
import com.github.mdr.mash.runtime.MashList

object ProcessesFunction extends MashFunction("os.processes") {

  private val processInteractions = LinuxProcessInteractions

  val params = ParameterModel()

  def apply(boundParams: BoundParams): MashList =
    MashList(processInteractions.getProcesses.map(ProcessClass.makeProcess))

  override def typeInferenceStrategy = Type.Seq(Type.Instance(ProcessClass))

  override def summaryOpt = Some("List processes")

}