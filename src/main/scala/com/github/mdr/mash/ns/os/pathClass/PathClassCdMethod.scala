package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type.unitToType
import com.github.mdr.mash.ns.os.CdFunction
import com.github.mdr.mash.runtime.MashUnit
import com.github.mdr.mash.runtime.MashValue

object PathClassCdMethod extends MashMethod("cd") {

  val params = ParameterModel()

  def apply(target: MashValue, arguments: Arguments): MashUnit = {
    params.validate(arguments)
    val path = FunctionHelpers.interpretAsPath(target)
    CdFunction.changeDirectory(path)
    MashUnit
  }

  override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Unit)

  override def summary = "Change directory to this path"

}