package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypeInferenceStrategy
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime._

object OpenFunction extends MashFunction("os.open") {

  object Params {
    val Item = Parameter(
      nameOpt = Some("item"),
      summaryOpt = Some("Item to open"))
  }
  import Params._

  val params = ParameterModel(Seq(Item))

  override def call(boundParams: BoundParams): MashUnit =
    open(boundParams(Item))

  def open(value: MashValue): MashUnit = {
    val process = new ProcessBuilder("open", ToStringifier.stringify(value)).start()
    process.waitFor()
    MashUnit
  }

  override def summaryOpt: Option[String] = Some("Open a given item with the default application")

  override def typeInferenceStrategy: TypeInferenceStrategy = UnitClass

}
