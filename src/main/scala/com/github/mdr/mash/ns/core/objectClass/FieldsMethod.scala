package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.FieldAndValueClass
import com.github.mdr.mash.ns.core.FieldAndValueClass.Fields._
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }

import scala.collection.immutable.ListMap

object FieldsMethod extends MashMethod("fields") {

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashList = {
    val obj = target.asInstanceOf[MashObject]
    MashList(getFieldObjects(obj))
  }

  def getFieldObjects(obj: MashObject): Seq[MashObject] = {
    def asObject(name: MashValue, value: MashValue) =
      MashObject.of(ListMap(
        Name -> name,
        Value -> value), FieldAndValueClass)
    obj.immutableFields.toSeq.map((asObject _).tupled)
  }

  override def typeInferenceStrategy = Seq(FieldAndValueClass)

  override def summaryOpt = Some("Return the fields of this object")

  override val isShy = true

}
