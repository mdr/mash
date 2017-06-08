package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ Inferencer, TypeInferenceStrategy, _ }
import com.github.mdr.mash.ns.core.NoArgFunction
import com.github.mdr.mash.ns.core.NoArgFunction.NoArgValue
import com.github.mdr.mash.runtime.{ MashList, MashNull, MashString, MashValue }

import scala.PartialFunction.condOpt

object FirstFunction extends MashFunction("collections.first") {

  object Params {
    val N: Parameter = Parameter(
      nameOpt = Some("n"),
      summaryOpt = Some("Number of elements"),
      defaultValueGeneratorOpt = Some(NoArgValue))
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence to find the first value(s) of"))
  }

  import Params._

  val params = ParameterModel(N, Sequence)

  def call(boundParams: BoundParams): MashValue = {
    boundParams.validateSequence(Sequence)
    val sequence = boundParams(Sequence)
    boundParams.validateIntegerOpt(N) match {
      case Some(count) ⇒
        if (count < 0)
          boundParams.throwInvalidArgument(N, s"Must be non-negative, but was $count")
        else
          sequence match {
            case s: MashString ⇒ s.modify(_ take count)
            case xs: MashList  ⇒ xs take count
          }
      case None        ⇒
        sequence match {
          case s: MashString ⇒ if (s.isEmpty) MashNull else s.first
          case xs: MashList  ⇒ if (xs.isEmpty) MashNull else xs.head
        }
    }
  }

  override def typeInferenceStrategy = FirstTypeInferenceStrategy

  override def summaryOpt = Some("Find the first element(s) of a sequence")

  override def descriptionOpt = Some(
    s"""If a count ${N.nameOpt} is provided, the first ${N.nameOpt} items of the sequence will be returned.
If there are fewer than ${N.nameOpt} in the sequence, the entire sequence is returned.
If a count ${N.nameOpt} is omitted, then the first item of the sequence is returned, if nonempty, else null.

Examples:
   first 3 [1, 2, 3, 4 5] # [1, 2, 3]
   first 5 [1, 2, 3]      # [1, 2, 3]
   first [1, 2, 3]        # 1
   first []               # null""")

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