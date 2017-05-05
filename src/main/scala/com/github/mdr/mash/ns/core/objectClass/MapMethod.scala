package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.runtime.{ MashObject, MashString, MashValue }

object MapMethod extends MashMethod("map") {

  object Params {
    val F = Parameter(
      nameOpt = Some("f"),
      summaryOpt = Some("Function used to transform fields of the object"),
      descriptionOpt = Some("The function must take two arguments, the field name and value."))
  }

  import Params._

  val params = ParameterModel(Seq(F))

  def call(target: MashValue, boundParams: BoundParams): MashObject = {
    val obj = target.asInstanceOf[MashObject]
    doMap(obj, boundParams)
  }

  def doMap(obj: MashObject, boundParams: BoundParams): MashObject = {
    val f = boundParams.validateFunction2(F)
    val objects = obj.immutableFields.map { case (field, value) ⇒ f(MashString(field), value).asInstanceOf[MashObject] }
    objects.reduceOption(_ + _) getOrElse MashObject.empty
  }

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt: Option[String] = Some("Return a new object with the fields transformed")

  override def descriptionOpt = Some(
    """Examples:"""".stripMargin)

  override val isShy = true

}
