package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.MemberEvaluator
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime._

object TransposeFunction extends MashFunction("collections.transpose") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence or Object to transpose"))
    val SkipGaps = Parameter(
      nameOpt = Some("skipGaps"),
      summaryOpt = Some("If true, skip gaps in lists where an entry isn't available; if false, use null (default false)"),
      shortFlagOpt = Some('s'),
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isFlag = true,
      isBooleanFlag = true)
  }

  import Params._

  val params = ParameterModel(Sequence, SkipGaps)

  def call(boundParams: BoundParams): MashValue = {
    val outer = boundParams(Sequence)
    val skipGaps = boundParams(SkipGaps).isTruthy
    outer match {
      case xs: MashList    ⇒ transposeList(boundParams, xs, skipGaps)
      case obj: MashObject ⇒ transposeObject(boundParams, obj)
      case _               ⇒ boundParams.throwInvalidArgument(Sequence, "Must be a List or Object")
    }
  }

  private def transposeObject(boundParams: BoundParams, obj: MashObject): MashValue =
    if (obj.immutableFields.forall(_._2.isAnObject)) {
      val objectsByField: Seq[(String, MashObject)] = obj.immutableFields.toSeq.map { case (field, value) ⇒
        field -> value.asInstanceOf[MashObject]
      }
      val allInnerFields = objectsByField.flatMap { case (_, innerObject) ⇒ innerObject.immutableFields.keys }.distinct
      MashObject.of {
        allInnerFields.map { innerField ⇒
          val newObj = MashObject.of(
            objectsByField.flatMap { case (outerField, innerObject) ⇒
              MemberEvaluator.maybeLookup(innerObject, innerField).map(outerField -> _)
            })
          innerField -> newObj
        }
      }
    } else if (obj.immutableFields.forall(_._2.isAList)) {
      val fields: Seq[(String, Seq[MashValue])] = obj.immutableFields.toSeq.map { case (field, value) ⇒
        field -> value.asInstanceOf[MashList].immutableElements
      }
      MashList(transposeObject(fields))
    } else
      boundParams.throwInvalidArgument(Sequence, "Object values must be either all Lists or Objects")


  private def transposeObject(fields: Seq[(String, Seq[MashValue])]): Seq[MashObject] = {
    if (fields.forall(_._2.isEmpty))
      return Seq()
    val headObject =
      MashObject.of(
        for {
          (field, values) ← fields
          value ← values.headOption
        } yield field -> value)
    val rest =
      for {
        (field, values) ← fields
        newValues = values.drop(1)
        if newValues.nonEmpty
      } yield field -> newValues
    headObject +: transposeObject(rest)
  }

  private def transposeList(boundParams: BoundParams, xs: MashList, skipGaps: Boolean): MashValue =
    if (xs.immutableElements.forall(_.isAList)) {
      val xss = xs.immutableElements.asInstanceOf[Seq[MashList]].map(_.immutableElements)
      MashList(transposeListOfLists(xss, skipGaps))
    } else if (xs.immutableElements.forall(_.isAnObject)) {
      val objects = xs.immutableElements.asInstanceOf[Seq[MashObject]]
      transposeListOfObjects(objects, skipGaps)
    } else
      boundParams.throwInvalidArgument(Sequence, "List must contain either all Lists or Objects")

  private def transposeListOfLists(xss: Seq[Seq[MashValue]], skipGaps: Boolean): Seq[MashList] =
    if (xss.forall(_.isEmpty))
      Seq()
    else
      MashList(xss.flatMap(_.headOption orElse Some(MashNull).filterNot(_ ⇒ skipGaps))) +: transposeListOfLists(xss.map(_ drop 1), skipGaps)

  private def transposeListOfObjects(objects: Seq[MashObject], skipGaps: Boolean): MashObject = {
    val allFields = objects.flatMap(_.immutableFields.keys).distinct
    def fieldList(field: String): MashList = {
      MashList(objects.flatMap { obj ⇒
        MemberEvaluator.maybeLookup(obj, field) orElse Some(MashNull).filterNot(_ ⇒ skipGaps)
      })
    }
    MashObject.of(allFields.map(f => f -> fieldList(f)))
  }

  override def summaryOpt = Some("Transpose a nested structure")

}
