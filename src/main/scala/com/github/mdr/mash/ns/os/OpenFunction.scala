package com.github.mdr.mash.ns.os

import com.github.mdr.mash.evaluator.{ MemberEvaluator, ToStringifier }
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.TypeInferenceStrategy
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime._
import org.apache.commons.lang3.SystemUtils

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
      MemberEvaluator.maybeLookupByString(item, "open") match {
        case Some(NullaryCallable(nc)) ⇒ nc.callNullary()
        case _                         ⇒ openWithSystemOpener(item)
      }
    MashUnit
  }

  def openWithSystemOpener(value: MashValue): MashUnit = {
    val argument = ToStringifier.stringify(value)
    val builder = if (SystemUtils.IS_OS_MAC_OSX) new ProcessBuilder("open", argument) else new ProcessBuilder("xdg-open", argument)
    val process = builder.start()
    process.waitFor()
    MashUnit
  }

  override def summaryOpt: Option[String] = Some("Open a given item with the default application")

  override def typeInferenceStrategy: TypeInferenceStrategy = UnitClass

}
