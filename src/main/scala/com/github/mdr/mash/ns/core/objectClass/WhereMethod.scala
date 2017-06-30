package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.ns.core.objectClass.MapMethod.getFieldValueIndexTriples
import com.github.mdr.mash.runtime.{ MashNumber, MashObject, MashValue }

object WhereMethod extends MashMethod("where") {

  object Params {
    val Predicate = Parameter(
      nameOpt = Some("predicate"),
      summaryOpt = Some("Predicate used to test fields"),
      descriptionOpt = Some(
        """May take up to three positional arguments:
          |  1) the field name
          |  2) the field value
          |  3) the index of the field""".stripMargin))
  }

  import Params._

  val params = ParameterModel(Predicate)

  def call(target: MashValue, boundParams: BoundParams): MashObject = {
    val obj = target.asInstanceOf[MashObject]
    val predicate = validatePredicate(boundParams)
    MashObject.of(
      getFieldValueIndexTriples(obj)
        .filter(predicate.tupled)
        .map { case (field, value, _) ⇒ field -> value })
  }

  def validatePredicate(boundParams: BoundParams): (MashValue, MashValue, MashNumber) ⇒ Boolean =
    boundParams.validateFunction1Or2Or3(Predicate) match {
      case Function1Or2Or3.One(f1)   ⇒ (field, value, i) ⇒ f1(field).isTruthy
      case Function1Or2Or3.Two(f2)   ⇒ (field, value, i) ⇒ f2(field, value).isTruthy
      case Function1Or2Or3.Three(f3) ⇒ (field, value, i) ⇒ f3(field, value, i).isTruthy
    }

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt: Option[String] = Some("Return a new object retaining only fields satisfying the given predicate")

  override def descriptionOpt = Some(
    """Examples:
      |  { "foo": 1, "bar": 2, "baz": 3 }.where (.startsWith "b")                   # { "bar": 2, "baz": 3 }
      |  { "foo": 1, "bar": 2, "baz": 3 }.where (f v => f.startsWith "f" or v == 3) # { "foo": 1, "baz": 3 }""".stripMargin)

  override val isShy = true

}
