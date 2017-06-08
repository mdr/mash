package com.github.mdr.mash.ns.core.thread

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime.{ MashUnit, MashValue }

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future

object InBackgroundFunction extends MashFunction("core.thread.inBackground") {

  object Params {
    val Block = Parameter(
      nameOpt = Some("block"),
      summaryOpt = Some("Code to execute"),
      isLazy = true)
  }

  import Params._

  val params = ParameterModel(Block)

  def call(boundParams: BoundParams): MashValue = {
    val f = boundParams(Block).asInstanceOf[MashFunction]
    Future {
      f.callNullary()
    }
    MashUnit
  }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Fork off the given code in the background")

}
