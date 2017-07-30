package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }

object ValuesMethod extends MashMethod("values") {

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashList = {
    val obj = target.asInstanceOf[MashObject]
    MashList(obj.immutableFields.toSeq.map(_._2))
  }

  override def typeInferenceStrategy = Seq(AnyClass)

  override def summaryOpt = Some("Return the values of the fields of this object, as a List")

  override val isShy = true

}
