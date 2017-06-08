package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator.{ MemberEvaluator, ToStringifier }
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.TypeInferenceStrategy
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime._

object OpenFunction extends MashFunction("os.open") {

  object Params {
    val Items = Parameter(
      nameOpt = Some("items"),
      summaryOpt = Some("Items to open"),
      isVariadic = true,
      variadicFlatten = true,
      variadicAtLeastOne = true)
  }

  import Params._

  val params = ParameterModel(Items)

  override def call(boundParams: BoundParams): MashUnit = {
    val items = boundParams.validateSequence(Items)
    for (item ← items)
      MemberEvaluator.maybeLookup(item, "open") match {
        case Some(NullaryCallable(nc)) ⇒ nc.callNullary()
        case _                         ⇒ openWithSystemOpener(item)
      }
    MashUnit
  }

  def openWithSystemOpener(value: MashValue): MashUnit = {
    val process = new ProcessBuilder("open", ToStringifier.stringify(value)).start()
    process.waitFor()
    MashUnit
  }

  override def summaryOpt: Option[String] = Some("Open a given item with the default application")

  override def typeInferenceStrategy: TypeInferenceStrategy = UnitClass

}
