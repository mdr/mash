package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.runtime.{ MashObject, MashString, MashValue }

object WhereMethod extends MashMethod("where") {

  object Params {
    val Predicate = Parameter(
      nameOpt = Some("predicate"),
      summaryOpt = Some("Predicate used to test fields"),
      descriptionOpt = Some("If the function can take one argument, the field name is supplied. If it can take two, the field name and value are supplied."))
  }

  import Params._

  val params = ParameterModel(Seq(Predicate))

  def apply(target: MashValue, boundParams: BoundParams): MashObject = {
    val obj = target.asInstanceOf[MashObject]
    doWhere(obj, boundParams)
  }

  def doWhere(obj: MashObject, boundParams: BoundParams): MashObject = {
    val test: (String, MashValue) ⇒ Boolean = validatePredicate(boundParams)
    MashObject.of(
      for ((field, value) <- obj.immutableFields if test(field, value))
        yield (field, value))
  }

  def validatePredicate(boundParams: BoundParams): (String, MashValue) ⇒ Boolean = {
    boundParams.validateFunction1Or2(Predicate) match {
      case Left(f)  ⇒ (field: String, value: MashValue) ⇒ f(MashString(field)).isTruthy
      case Right(f) ⇒ (field: String, value: MashValue) ⇒ f(MashString(field), value).isTruthy
    }
  }

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt: Option[String] = Some("Return a new object retaining only fields satisfying the given predicate")

  override def descriptionOpt = Some(
    """Examples:
      |  { "foo": 1, "bar": 2, "baz": 3 }.where (.startsWith "b")                   # { "bar": 2, "baz": 3 }
      |  { "foo": 1, "bar": 2, "baz": 3 }.where (f v => f.startsWith "f" or v == 3) # { "foo": 1, "baz": 3 }""".stripMargin)
}
