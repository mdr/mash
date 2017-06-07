package com.github.mdr.mash.ns.collections.listClass

import com.github.mdr.mash.evaluator.{ EvaluatorException, MemberEvaluator }
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashString, MashValue }

object InnerJoinMethod extends MashMethod("innerJoin") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("A list of other Objects to join against"))
    val On = Parameter(
      nameOpt = Some("on"),
      summaryOpt = Some("A predicate to join on"))
    val Prefix1 = Parameter(
      nameOpt = Some("prefix1"),
      summaryOpt = Some("Prefix to use for fields from this list of objects"),
      isFlag = true,
      defaultValueGeneratorOpt = Some(MashString("")))
    val Prefix2 = Parameter(
      nameOpt = Some("prefix2"),
      summaryOpt = Some("Prefix to use for fields from the given list of objects"),
      isFlag = true,
      defaultValueGeneratorOpt = Some(MashString("")))
  }

  import Params._

  override val params = ParameterModel(Seq(Sequence, On, Prefix1, Prefix2))

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
    val on = boundParams.validateFunction2(On)
    val prefix1 = boundParams.validateString(Prefix1).s
    val prefix2 = boundParams.validateString(Prefix2).s
    MashList(
      for {
        object1 ← objects1
        object2 ← objects2
        if on(object1, object2).isTruthy
      } yield addPrefix(object1, prefix1) + addPrefix(object2, prefix2))
  }

  private def addPrefix(obj: MashObject, prefix: String): MashObject =
    MashObject.of(
      for ((f, v) <- obj.immutableFields)
        yield (prefix + f) -> v)

  override def summaryOpt: Option[String] = Some("Inner join with a list of other objects")

}
