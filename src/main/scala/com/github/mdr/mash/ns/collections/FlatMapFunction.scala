package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.{ Arguments, EvaluatorException }
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.{ MashBoolean, MashList, MashNumber, MashValue }

import scala.PartialFunction.condOpt

object FlatMapFunction extends MashFunction("collections.flatMap") {

  object Params {
    val F = Parameter(
      name = "f",
      summary = "Function used to transform elements of the sequence",
      descriptionOpt = Some("Must return a sequence"))
    val WithIndex = Parameter(
      name = "withIndex",
      shortFlagOpt = Some('i'),
      summary = "Pass index into the function as well as the item",
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isFlag = true,
      isBooleanFlag = true)
    val Sequence = Parameter(
      name = "sequence",
      summary = "Sequence to map over",
      isLast = true)
  }

  import Params._

  val params = ParameterModel(Seq(F, Sequence, WithIndex))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val withIndex = boundParams(WithIndex).isTruthy
    val sequence = boundParams.validateSequence(Sequence)
    def getItems(value: MashValue) = value match {
      case MashList(items @ _*) ⇒
        items
      case badItem            ⇒
        throw new EvaluatorException("Invalid item of type " + badItem.typeName)
    }
    val mapped =
      if (withIndex) {
        val f = boundParams.validateFunction2(F)
        sequence.zipWithIndex.flatMap { case (item, index) ⇒
          getItems(f(item, MashNumber(index)))
        }
      } else {
        val f = boundParams.validateFunction(F)
        sequence.flatMap { item ⇒
          getItems(f(item))
        }
      }
    MashList(mapped)
  }

  override def typeInferenceStrategy = FlatMapTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = {
    val argBindings = FlatMapFunction.params.bindTypes(arguments)
    val specOpt =
      for {
        param ← argBindings.paramAt(argPos)
        if param == F
        Type.Seq(elementType) ← argBindings.getType(Sequence)
      } yield CompletionSpec.Members(elementType)
    specOpt.toSeq
  }

  override def summary = "Transform each element of a sequence by a given function, and then flatten"

  override def descriptionOpt = Some("""The given function is applied to each element of the input sequence 
  and is expected to yield a sequence for each element. The result is flattened to produce a sequence of transformed 
  output elements.

Examples:
  flatMap (x => [x * 10, x * 100]) [1, 2, 3] # [20, 200, 40, 400, 60, 600]""")

}

object FlatMapTypeInferenceStrategy extends TypeInferenceStrategy {

  import FlatMapFunction.Params._

    def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = FlatMapFunction.params.bindTypes(arguments)
    val functionOpt = argBindings.getArgument(F)
    val sequenceTypeOpt = argBindings.getType(Sequence)
    val newElementSeqTypeOpt = MapTypeInferenceStrategy.inferAppliedType(inferencer, functionOpt, sequenceTypeOpt)
    for {
      sequenceType ← sequenceTypeOpt
      newSequenceType ← condOpt(sequenceType) {
        case Type.Seq(_) ⇒
          newElementSeqTypeOpt match {
            case Some(Type.Seq(newElementType)) ⇒ newElementType.seq
            case _                              ⇒ Type.Any.seq
          }
      }
    } yield newSequenceType
  }

}