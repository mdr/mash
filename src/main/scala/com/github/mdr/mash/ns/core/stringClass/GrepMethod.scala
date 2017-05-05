package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ Inferencer, MethodTypeInferenceStrategy, Type, TypedArguments }
import com.github.mdr.mash.ns.collections.GrepFunction
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object GrepMethod extends MashMethod("grep") {
  import GrepFunction.Params._

  val params = ParameterModel(Seq(Query, IgnoreCase, Regex, Negate))

  override def call(target: MashValue, boundParams: BoundParams): MashValue = {
    val ignoreCase = boundParams(IgnoreCase).isTruthy
    val regex = boundParams(Regex).isTruthy
    val negate = boundParams(Negate).isTruthy
    val query = ToStringifier.stringify(boundParams(Query))
    val items = GrepFunction.getItems(target.asInstanceOf[MashString])
    GrepFunction.runGrep(items, query, ignoreCase, regex, negate)
  }

  override object typeInferenceStrategy extends MethodTypeInferenceStrategy {
    def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] =
      targetTypeOpt.map(_.seq)
  }

  override def summaryOpt: Option[String] = Some("Find all the elements in the lines of this String which match the given query")
}
