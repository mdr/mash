package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type.unitToType
import com.github.mdr.mash.ns.os.ChangeDirectoryFunction
import com.github.mdr.mash.runtime.MashUnit
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.evaluator.EvaluatorException

object PathClassCdMethod extends MashMethod("cd") {

  val params = ParameterModel()

  def apply(target: MashValue, arguments: Arguments): MashUnit = {
    params.validate(arguments)
    val path = FunctionHelpers.interpretAsPath(target)
    import ChangeDirectoryFunction._
    changeDirectory(path) match {
      case Success ⇒
        MashUnit
      case NotADirectory ⇒
        throw new EvaluatorException(s"Could not change directory to '$path', not a directory")
    }
    MashUnit
  }

  override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Unit)

  override def summary = "Change directory to this path"

}