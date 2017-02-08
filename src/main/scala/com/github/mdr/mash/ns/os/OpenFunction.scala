package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator.{ Arguments, ToStringifier }
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
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

  override def apply(arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
    val item = boundParams(Item)
    val process = new ProcessBuilder("open", ToStringifier.stringify(item)).start()
    process.waitFor()
    MashUnit
  }

  override def summaryOpt: Option[String] = Some("Open a given item with the default application")

  override def typeInferenceStrategy: TypeInferenceStrategy = UnitClass

}
