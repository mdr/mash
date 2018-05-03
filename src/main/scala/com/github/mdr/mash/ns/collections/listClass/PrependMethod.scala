package com.github.mdr.mash.ns.collections.listClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.collections.listClass.AppendMethod.AppendMethodTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashList, MashValue }

object PrependMethod extends MashMethod("prepend") {

  object Params {
    val Item = Parameter(
      nameOpt = Some("item"),
      summaryOpt = Some("An item to place at the start of the List"))
  }

  import Params._

  override val params = ParameterModel(Item)

  def call(target: MashValue, boundParams: BoundParams): MashList =
    MashList(boundParams(Item) +: target.asInstanceOf[MashList].immutableElements)

  override def summaryOpt: Option[String] = Some("Prepend the given item to the start of this List")

  override def typeInferenceStrategy = AppendMethodTypeInferenceStrategy

}
