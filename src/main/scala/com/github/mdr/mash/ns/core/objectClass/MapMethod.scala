package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.collections.FlatMapFunction
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString, MashValue }

object MapMethod extends MashMethod("map") {

  object Params {
    val F = Parameter(
      nameOpt = Some("f"),
      summaryOpt = Some("Function used to transform fields of the object"),
      descriptionOpt = Some(
        """The function may take up to three positional arguments:
          |  1) the field name,
          |  2) the field value,
          |  3) the index of the field.
          |The function may return Objects, in which case they are merged into a single output Object.
          |If it returns other types of values, then the map will return them in a List.""".stripMargin))
  }

  import Params._

  val params = ParameterModel(F)

  def call(target: MashValue, boundParams: BoundParams): MashValue = {
    val obj = target.asInstanceOf[MashObject]
    doMap(obj, boundParams)
  }

  def doMap(obj: MashObject, boundParams: BoundParams): MashValue = {
    val f = boundParams.validateFunction1Or2Or3(F)
    val fieldValueIndexTriples =
      FlatMapFunction.zipWithMashIndex(obj.immutableFields)
        .map { case ((field, value), i) ⇒ (MashString(field), value, i) }
    val mappedValues = fieldValueIndexTriples.map { case (field, value, i) ⇒
      f match {
        case Function1Or2Or3.One(f1)   ⇒ f1(field)
        case Function1Or2Or3.Two(f2)   ⇒ f2(field, value)
        case Function1Or2Or3.Three(f3) ⇒ f3(field, value, i)
      }
    }
    if (mappedValues.isEmpty)
      MashObject.empty
    else if (mappedValues.forall(_.isAnObject))
      MashObject.merge(mappedValues.asInstanceOf[Seq[MashObject]])
    else
      MashList(mappedValues)

  }

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt: Option[String] = Some("Return a new object with the fields transformed")

  override def descriptionOpt = Some(
    """Examples:
      |  { apple: 1, bob: 2, cat: 3 }.map (f v => { (f.toUpper): v * v })  # { APPLE: 1, BOB: 4, CAT: 9 }
    """.stripMargin)

  override val isShy = true

}
