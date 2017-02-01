package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
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

  val params = ParameterModel(Seq(Sequence1, Sequence2))

  def apply(arguments: Arguments): MashList = {
    val boundParams = params.validate(arguments)
    MashList(for ((l, r) <- boundParams.validateSequence(Sequence1) zip boundParams.validateSequence(Sequence2))
      yield MashList.of(l, r))
  }

  override def summaryOpt = Some("Zip two sequences")

  override def descriptionOpt = Some("""Examples:
  zip [1, 2, 3] [4, 5] # [[1, 4], [2, 5]]""")

}