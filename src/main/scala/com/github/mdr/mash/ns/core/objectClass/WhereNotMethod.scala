package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.ns.core.objectClass.MapMethod.getFieldValueIndexTriples
import com.github.mdr.mash.ns.core.objectClass.WhereMethod.validatePredicate
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

object WhereNotMethod extends MashMethod("whereNot") {

  import WhereMethod.Params._

  val params = ParameterModel(Predicate)

  def call(target: MashValue, boundParams: BoundParams): MashObject =
    doWhereNot(target.asInstanceOf[MashObject], boundParams)

  def doWhereNot(obj: MashObject, boundParams: BoundParams): MashObject = {
    val predicate = validatePredicate(boundParams)
    MashObject.of(
      getFieldValueIndexTriples(obj)
        .filterNot(predicate.tupled)
        .map { case (field, value, _) ⇒ field -> value })
  }

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt: Option[String] = Some("Return a new object without any fields satisfying the given predicate")

  override def descriptionOpt = Some(
    """Examples:
      |  { "foo": 1, "bar": 2, "baz": 3 }.whereNot (.startsWith "b")                   # { "foo": 1 }
      |  { "foo": 1, "bar": 2, "baz": 3 }.whereNot (f v => f.startsWith "f" or v == 3) # { "bar": 2 }""".stripMargin)

  override val isShy = true

}
