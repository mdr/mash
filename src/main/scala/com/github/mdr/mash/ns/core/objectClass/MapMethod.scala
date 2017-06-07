package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.runtime.{ MashObject, MashString, MashValue }

object MapMethod extends MashMethod("map") {

  object Params {
    val F = Parameter(
      nameOpt = Some("f"),
      summaryOpt = Some("Function used to transform fields of the object"),
      descriptionOpt = Some("The function must take two arguments, the field name and value, and return an Object"))
  }

  import Params._

  val params = ParameterModel(Seq(F))

  def call(target: MashValue, boundParams: BoundParams): MashObject = {
    val obj = target.asInstanceOf[MashObject]
    doMap(obj, boundParams)
  }

  def doMap(obj: MashObject, boundParams: BoundParams): MashObject = {
    val f = boundParams.validateFunction2(F)
    val objects = obj.immutableFields.map { case (field, value) ⇒
      f(MashString(field), value) match {
        case obj: MashObject ⇒ obj
        case x               ⇒ throw new EvaluatorException(s"Transformed value must be an Object, but was a ${x.typeName}")
      }
    }
    objects.reduceOption(_ + _) getOrElse MashObject.empty
  }

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt: Option[String] = Some("Return a new object with the fields transformed")

  override def descriptionOpt = Some(
    """Examples:
      |  { apple: 1, bob: 2, cat: 3 }.map (f v => { (f.toUpper): v * v })  # { APPLE: 1, BOB: 4, CAT: 9 }
    """.stripMargin)

  override val isShy = true

}
