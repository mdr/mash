package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.{ MashList, MashValue }

class StringFunction(s: String,
                     functionLocationOpt: Option[SourceLocation],
                     invocationLocationOpt: Option[SourceLocation]) extends MashFunction() {

  object Params {
    val Target = Parameter(
      nameOpt = Some("target"),
      summaryOpt = Some("Target value to lookup a member"))
  }

  import Params._

  val params = ParameterModel(Seq(Target))

  def apply(boundParams: BoundParams): MashValue =
    boundParams(Target) match {
      case xs: MashList ⇒ xs.map(lookupMember)
      case target       ⇒ lookupMember(target)
    }

  private def lookupMember(target: MashValue): MashValue = {
    val intermediateResult = MemberEvaluator.lookup(target, s, locationOpt = functionLocationOpt)
    Evaluator.invokeNullaryFunctions(intermediateResult, invocationLocationOpt)
  }

  override def summaryOpt = Some("String as a function")

}
