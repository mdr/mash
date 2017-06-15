package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.collections.FlatMapFunction.zipWithMashIndex
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.runtime._

object MapMethod extends MashMethod("map") {

  object Params {
    val F = Parameter(
      nameOpt = Some("f"),
      summaryOpt = Some("Function used to transform fields of the object"),
      descriptionOpt = Some(
        """The function may take up to three positional arguments:
          |  1) the field name
          |  2) the field value
          |  3) the index of the field
          |The function may return Objects, in which case they are merged into a single output Object.
          |If it returns other types of values, then the map will return them in a List.""".stripMargin))
  }

  import Params._

  val params = ParameterModel(F)

  def call(target: MashValue, boundParams: BoundParams): MashValue =
    doMap(target.asInstanceOf[MashObject], boundParams)

  def doMap(obj: MashObject, boundParams: BoundParams): MashValue = {
    val f = validateMappingFunction(boundParams)
    val mappedValues = getFieldValueIndexTriples(obj).map(f.tupled)
    constructResult(mappedValues)
  }

  private def validateMappingFunction(boundParams: BoundParams): (MashString, MashValue, MashNumber) ⇒ MashValue =
    boundParams.validateFunction1Or2Or3(F) match {
      case Function1Or2Or3.One(f1)   ⇒ (field, value, i) ⇒ f1(field)
      case Function1Or2Or3.Two(f2)   ⇒ (field, value, i) ⇒ f2(field, value)
      case Function1Or2Or3.Three(f3) ⇒ (field, value, i) ⇒ f3(field, value, i)
    }

  def getFieldValueIndexTriples(obj: MashObject): Seq[(MashString, MashValue, MashNumber)] =
    zipWithMashIndex(obj.immutableFields)
      .map { case ((field, value), i) ⇒ (MashString(field), value, i) }

  private def constructResult(mappedValues: Seq[MashValue]): MashValue =
    if (mappedValues.isEmpty)
      MashObject.empty
    else if (mappedValues.forall(_.isAnObject))
      MashObject.merge(mappedValues.asInstanceOf[Seq[MashObject]])
    else
      MashList(mappedValues)

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt: Option[String] = Some("Return a new object with the fields transformed")

  override def descriptionOpt = Some(
    """Examples:
      |  { apple: 1, bob: 2, cat: 3 }.map (f v => { (f.toUpper): v * v })  # { APPLE: 1, BOB: 4, CAT: 9 }
    """.stripMargin)

  override val isShy = true

}
