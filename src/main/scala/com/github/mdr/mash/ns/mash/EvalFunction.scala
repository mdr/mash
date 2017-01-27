package com.github.mdr.mash.ns.mash

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.runtime.MashValue

object EvalFunction extends MashFunction("mash.eval") {

  private lazy val scriptExecutor = Singletons.scriptExecutor

  object Params {
    val Mash = Parameter(
      nameOpt = Some("mash"),
      summaryOpt = Some("Mash code"))
  }
  import Params._

  val params = ParameterModel(Seq(Mash))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val mash = boundParams.validateString(Mash).s
    scriptExecutor.runUnit(CompilationUnit(mash, "eval", mish = false))
  }

  override def summaryOpt = Some("Execute the given string as a Mash expression")

}