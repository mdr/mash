package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.MashList
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy

object SubprocessResultClass extends MashClass("os.subprocessResult") {

  object Fields {
    val ExitStatus = Field("exitStatus", "Exit status of process", Type.Instance(NumberClass))
    val Stdout = Field("stdout", "Captured stdout", Type.Instance(StringClass))
  }

  import Fields._

  override val fields = Seq(ExitStatus, Stdout)

  def summary = "The result of running a subprocess"

  override val methods = Seq(
    LinesMethod)

  case class Wrapper(x: Any) {
    val obj = x.asInstanceOf[MashObject]
    def stdout = obj(Stdout).asInstanceOf[MashString].s
  }

  object LinesMethod extends MashMethod("lines") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashList = {
      params.validate(arguments)
      MashList(Wrapper(target).stdout.split("\n").map(MashString(_)))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Seq(Type.Instance(StringClass)))

    override def summary = "The standard output of the process as as sequence of lines"

  }

}