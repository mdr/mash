package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.{ MashNumber, MashUnit }

object ExitFunction extends MashFunction("core.exit") {

  object Params {
    val Status = Parameter(
      nameOpt = Some("status"),
      summaryOpt = Some("Exit status (default 0)"),
      defaultValueGeneratorOpt = Some(() ⇒ MashNumber(0)))
  }
  import Params._

  val params = ParameterModel(Seq(Status))

  def apply(arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
    val status = boundParams.validateInteger(Status)
    System.exit(status)
    MashUnit
  }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Exit mash")

}