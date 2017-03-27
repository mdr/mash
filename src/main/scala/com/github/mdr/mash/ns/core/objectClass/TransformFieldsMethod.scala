package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.runtime.{ MashObject, MashString, MashValue }

object TransformFieldsMethod extends MashMethod("transformFields") {

  object Params {
    val F = Parameter(
      nameOpt = Some("f"),
      summaryOpt = Some("Function used to transform field names of the object"),
      descriptionOpt = Some(
        """The function may take up to two arguments. If it takes only one argument, the field name is provided.
          |  If takes two, the field name and value are provided.""".stripMargin))
  }

  import Params._

  val params = ParameterModel(Seq(F))

  def apply(target: MashValue, boundParams: BoundParams): MashObject = {
    val obj = target.asInstanceOf[MashObject]
    boundParams.validateFunction1Or2(F) match {
      case Left(f)  ⇒
        MashObject.of(obj.immutableFields.map { case (name, value) ⇒
          ToStringifier.stringify(f(MashString(name))) -> value
        })
      case Right(f) ⇒
        MashObject.of(obj.immutableFields.map { case (name, value) ⇒
          ToStringifier.stringify(f(MashString(name), value)) -> value
        })
    }
  }

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt: Option[String] = Some("Return a new object with the field names transformed")

  override def descriptionOpt = Some(
    """Examples:
      |  { foo: 3, bar: 4 }.transformFields (.toUpper) # { FOO: 3, BAR: 4 }""".stripMargin)
}
