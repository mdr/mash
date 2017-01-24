package com.github.mdr.mash.ns.core.thread

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashUnit, MashValue }

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future

object InBackgroundFunction extends MashFunction("core.thread.inBackground") {

  object Params {
    val Block = Parameter(
      nameOpt = Some("block"),
      summary = "Code to execute",
      isLazy = true)
  }

  import Params._

  val params = ParameterModel(Seq(Block))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val f = boundParams(Block).asInstanceOf[MashFunction]
    Future {
      f.apply(Arguments(Seq()))
    }
    MashUnit
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summaryOpt = Some("Fork off the given code in the background")

}
