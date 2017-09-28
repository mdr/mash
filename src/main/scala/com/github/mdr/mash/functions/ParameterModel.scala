package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.{ Arguments, EvaluationContext }
import com.github.mdr.mash.inference.TypedArguments

object ParameterModel {

  val Empty = ParameterModel()

  def apply(param1: Parameter, params: Parameter*): ParameterModel = ParameterModel(param1 +: params)

}

case class ParameterModel(params: Seq[Parameter] = Seq()) {

  require(params.count(_.isVariadic) <= 1)

  /**
    * Parameters where arguments can be provided positionally
    */
  val positionalParams: Seq[Parameter] = params.filterNot(p ⇒ p.isFlag || p.isNamedArgsParam)

  // Lookup parameter by name or short flag (if it has one)
  val paramByName: Map[String, Parameter] = {
    var paramMap: Map[String, Parameter] = Map()
    for (param ← params) {
      for (paramName ← param.nameOpt)
        paramMap += paramName -> param
      for (shortFlag ← param.shortFlagOpt)
        paramMap += shortFlag.toString -> param
    }
    paramMap
  }

  @throws[ArgumentException]
  def bindTo(arguments: Arguments, context: EvaluationContext): BoundParams =
    new ParamBindingContext(this, arguments, context).bind

  def flags: Seq[Flag] = params.map(param ⇒
    Flag(param.summaryOpt orElse param.nameOpt, param.shortFlagOpt.map(_.toString), param.nameOpt))

  def allowsNullary: Boolean = params forall canBeOmitted

  private def canBeOmitted(p: Parameter): Boolean =
    (p.isVariadic && !p.variadicAtLeastOne) ||
      p.defaultValueGeneratorOpt.isDefined ||
      p.isNamedArgsParam ||
      p.isAllArgsParam

  /**
    * Whether or not this can be called with two positional arguments
    */
  def allowsTwoPositionalArguments: Boolean = allowsAtLeastThisManyPositionalArguments(2)

  def allowsAtLeastThisManyPositionalArguments(count: Int): Boolean =
    params.exists(_.isVariadic) || params.exists(_.isAllArgsParam) || positionalParams.size >= count

  def bindTypes(arguments: TypedArguments): BoundTypeParams =
    new TypeParamBindingContext(this, arguments).bind()

  def isEmpty: Boolean = params.isEmpty

  def nonEmpty: Boolean = !isEmpty

}

