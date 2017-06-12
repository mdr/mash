package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.{ Inferencer, TypeInferenceStrategy, _ }
import com.github.mdr.mash.ns.core.NoArgFunction.NoArgValue
import com.github.mdr.mash.runtime._

import scala.PartialFunction.condOpt

object FirstFunction extends MashFunction("collections.first") {

  object Params {
    val N: Parameter = Parameter(
      nameOpt = Some("n"),
      summaryOpt = Some("Number of elements"),
      defaultValueGeneratorOpt = Some(NoArgValue))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to find the first value(s) of"),
      descriptionOpt = Some("Must be a List, String or Object"))
  }

  import Params._

  val params = ParameterModel(N, Sequence)

  def call(boundParams: BoundParams): MashValue = {
    val countOpt = validateCount(boundParams)
    boundParams(Sequence) match {
      case obj: MashObject ⇒
        ToListHelper.tryToList(obj) match {
          case Some(items) ⇒ first(items, countOpt)
          case None        ⇒ first(obj, countOpt)
        }
      case s: MashString   ⇒ first(s, countOpt)
      case xs: MashList    ⇒ first(xs.immutableElements, countOpt)
      case value           ⇒
        boundParams.throwInvalidArgument(Sequence, s"Must be a List, String, or Object, but was a ${value.typeName}")
    }
  }

  private def validateCount(boundParams: BoundParams): Option[Int] = {
    val countOpt = boundParams.validateIntegerOpt(N)
    for (count ← countOpt if count < 0)
      boundParams.throwInvalidArgument(N, s"Must be non-negative, but was $count")
    countOpt
  }

  private def first(s: MashString, countOpt: Option[Int]): MashValue =
    countOpt match {
      case Some(count) ⇒ s.modify(_ take count)
      case None        ⇒ if (s.isEmpty) MashNull else s.first
    }

  private def first(xs: Seq[MashValue], countOpt: Option[Int]): MashValue =
    countOpt match {
      case Some(count) ⇒ MashList(xs take count)
      case None        ⇒ if (xs.isEmpty) MashNull else xs.head
    }

  private def first(obj: MashObject, countOpt: Option[Int]): MashValue =
    countOpt match {
      case Some(count) ⇒ MashObject.of(obj.immutableFields take count)
      case None        ⇒ if (obj.isEmpty) MashNull else MashObject.of(obj.immutableFields take 1)
    }

  override def typeInferenceStrategy = FirstTypeInferenceStrategy

  override def summaryOpt = Some("Find the first element(s) of a sequence")

  override def descriptionOpt = Some(
    s"""If a count ${N.nameOpt} is provided, the first ${N.nameOpt} items of the sequence will be returned.
If there are fewer than ${N.nameOpt} in the sequence, the entire sequence is returned.
If a count ${N.nameOpt} is omitted, then the first item of the sequence is returned, if nonempty, else null.

Examples:
  first 3 [1, 2, 3, 4 5]  # [1, 2, 3]
  first 5 [1, 2, 3]       # [1, 2, 3]
  first [1, 2, 3]         # 1
  first []                # null
  first 3 'abcdef'        # 'abc'""")

}

object FirstTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = FirstFunction.params.bindTypes(arguments)
    import FirstFunction.Params._
    if (argBindings contains N)
      argBindings.getType(Sequence)
    else
      for {
        sequenceType ← argBindings.getType(Sequence)
        elementType ← condOpt(sequenceType) {
          case Type.Seq(elementType)      ⇒ elementType
          case Type.Patterns.AnyString(_) ⇒ sequenceType
        }
      } yield elementType
  }

}