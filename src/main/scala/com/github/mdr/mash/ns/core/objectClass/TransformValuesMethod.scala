package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

object TransformValuesMethod extends MashMethod("transformValues") {

  object Params {
    val F = Parameter(
      nameOpt = Some("f"),
      summaryOpt = Some("Function used to transform values of the object"),
      descriptionOpt = Some(
        """The function may take up to two arguments. If it takes only one argument, the value is provided.
          |  If takes two, the field name and value are provided.""".stripMargin))
  }

  import Params._

  val params = ParameterModel(F)

  def call(target: MashValue, boundParams: BoundParams): MashObject = {
    val obj = target.asInstanceOf[MashObject]
    boundParams.validateFunction1Or2(F) match {
      case Left(f)  ⇒ MashObject.of(obj.immutableFields.mapValues(f))
      case Right(f) ⇒ MashObject.of(obj.immutableFields.map { case (name, value) ⇒ name -> f(name, value) })
    }
  }

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt: Option[String] = Some("Return a new object with the values transformed")

  override def descriptionOpt = Some(
    """Examples:
      |  { foo: 3, bar: 4 }.transformValues (n => n * n) # { foo: 9, bar: 16 }""".stripMargin)

  override val isShy = true

}
