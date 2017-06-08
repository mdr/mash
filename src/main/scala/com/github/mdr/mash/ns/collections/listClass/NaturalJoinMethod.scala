package com.github.mdr.mash.ns.collections.listClass

import com.github.mdr.mash.evaluator.{ EvaluatorException, MemberEvaluator }
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }

object NaturalJoinMethod extends MashMethod("naturalJoin") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("A list of other Objects to join against"))
  }

  import Params._

  override val params = ParameterModel(Sequence)

  def call(target: MashValue, boundParams: BoundParams): MashList = {
    val objects1: Seq[MashObject] =
      target.asInstanceOf[MashList].immutableElements.map {
        case obj: MashObject ⇒ obj
        case value           ⇒ throw new EvaluatorException(s"Can only call this on a list of Objects, but a value was ${value.typeName}")
      }
    val objects2: Seq[MashObject] =
      boundParams.validateSequence(Sequence).map {
        case obj: MashObject ⇒ obj
        case value           ⇒ boundParams.throwInvalidArgument(Sequence, s"Can only call this on a list of Objects, but a value was ${value.typeName}")
      }
    val fields1 = objects1.flatMap(_.immutableFields.keys).distinct
    val fields2 = objects2.flatMap(_.immutableFields.keys).distinct
    val commonFields = fields1 intersect fields2
    if (commonFields.isEmpty)
      MashList.empty
    else {
      def groupObjectsByCommonFields(objects: Seq[MashObject]): Map[Seq[Option[MashValue]], Seq[MashObject]] =
        objects.groupBy { obj ⇒ commonFields.map(field ⇒ MemberEvaluator.maybeLookup(obj, field)) }
      val groups1 = groupObjectsByCommonFields(objects1)
      val groups2 = groupObjectsByCommonFields(objects2)

      val sharedTuples: Set[Seq[Option[MashValue]]] = groups1.keySet intersect groups2.keySet

      MashList(for {
        sharedTuple ← sharedTuples.toSeq
        if sharedTuple.forall(_.nonEmpty)
        obj1 ← groups1(sharedTuple)
        obj2 ← groups2(sharedTuple)
      } yield obj1 + obj2)
    }
  }

  override def summaryOpt: Option[String] = Some("Natural join with a list of other objects")

}
