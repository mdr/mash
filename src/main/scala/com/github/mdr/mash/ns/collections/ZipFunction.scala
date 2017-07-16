package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ Inferencer, Type, TypeInferenceStrategy, TypedArguments }
import com.github.mdr.mash.runtime.MashList

object ZipFunction extends MashFunction("collections.zip") {

  object Params {
    val Sequence1 = Parameter(
      nameOpt = Some("sequence1"),
      summaryOpt = Some("First sequence"))
    val Sequence2 = Parameter(
      nameOpt = Some("sequence2"),
      summaryOpt = Some("Second sequence"))
  }

  import Params._

  val params = ParameterModel(Sequence1, Sequence2)

  def call(boundParams: BoundParams): MashList = {
    MashList(for ((l, r) ← boundParams.validateSequence(Sequence1) zip boundParams.validateSequence(Sequence2))
      yield MashList.of(l, r))
  }

  override def summaryOpt = Some("Zip two sequences")

  override def descriptionOpt = Some(
    """Examples:
  zip [1, 2, 3] [4, 5] # [[1, 4], [2, 5]]""")

  override object typeInferenceStrategy extends TypeInferenceStrategy {

    override def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
      val argBindings = params.bindTypes(arguments)
      val element1TypeOpt = argBindings.getType(Sequence1).collect { case Type.Seq(elementType) ⇒ elementType }
      val element2TypeOpt = argBindings.getType(Sequence2).collect { case Type.Seq(elementType) ⇒ elementType }
      val pairType = element1TypeOpt orElse element2TypeOpt getOrElse Type.Any
      Some(pairType.seq.seq)
    }

  }

}