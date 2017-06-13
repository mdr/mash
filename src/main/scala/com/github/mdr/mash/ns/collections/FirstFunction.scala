package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.{ Inferencer, TypeInferenceStrategy, _ }
import com.github.mdr.mash.ns.core.NoArgFunction.NoArgValue
import com.github.mdr.mash.ns.core.ObjectClass
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
      descriptionOpt = Some("Can be a List, String or Object"))
  }

  import Params._

  val params = ParameterModel(N, Sequence)

  def call(boundParams: BoundParams): MashValue = {
    val countOpt = boundParams.validateNonNegativeIntegerOpt(N)
    SequenceLikeAnalyser.analyse(boundParams, Sequence) {
      case SequenceLike.Items(items) ⇒ countOpt match {
        case Some(count) ⇒ MashList(items take count)
        case None        ⇒ if (items.isEmpty) MashNull else items.head
      }
      case SequenceLike.String(s)    ⇒ countOpt match {
        case Some(count) ⇒ s.modify(_ take count)
        case None        ⇒ if (s.isEmpty) MashNull else s.first
      }
      case SequenceLike.Object(obj)  ⇒ countOpt match {
        case Some(count) ⇒ MashObject.of(obj.immutableFields take count)
        case None        ⇒ if (obj.isEmpty) MashNull else MashObject.of(obj.immutableFields take 1)
      }
    }
  }

  override def typeInferenceStrategy = FirstLastTypeInferenceStrategy(params, Sequence, N)

  override def summaryOpt = Some("Find the first element(s) of a sequence")

  override def descriptionOpt = Some(
    s"""If a count ${N.name} is provided, the first ${N.name} items of the sequence will be returned.
If there are fewer than ${N.name} in the sequence, the entire sequence is returned.
If a count ${N.name} is omitted, then the first item of the sequence is returned, if nonempty, else null.

Examples:
  first 3 [1, 2, 3, 4 5]  # [1, 2, 3]
  first 5 [1, 2, 3]       # [1, 2, 3]
  first [1, 2, 3]         # 1
  first []                # null
  first 3 "abcdef"        # "abc""")

}

case class FirstLastTypeInferenceStrategy(params: ParameterModel,
                                          sequenceParam: Parameter,
                                          countParam: Parameter) extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = params.bindTypes(arguments)
    if (argBindings contains countParam)
      argBindings.getType(sequenceParam) map {
        case instance@Type.Instance(klass) if klass isSubClassOf ObjectClass ⇒ instance.unbless
        case instance: Type.UserClassInstance                                ⇒ instance.unbless
        case type_                                                           ⇒ type_
      }
    else
      for {
        sequenceType ← argBindings.getType(sequenceParam)
        elementType ← condOpt(sequenceType) {
          case Type.Seq(elementType)      ⇒ elementType
          case Type.Patterns.AnyString(_) ⇒ sequenceType
        }
      } yield elementType
  }

}