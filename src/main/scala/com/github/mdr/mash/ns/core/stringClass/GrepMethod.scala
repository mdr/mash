package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ Inferencer, MethodTypeInferenceStrategy, Type, TypedArguments }
import com.github.mdr.mash.ns.collections.GrepFunction
import com.github.mdr.mash.ns.collections.GrepFunction.GrepOptions
import com.github.mdr.mash.runtime.{ MashString, MashValue }
import com.github.mdr.mash.utils.Utils._

object GrepMethod extends MashMethod("grep") {
  import GrepFunction.Params._

  val params = ParameterModel(Query, IgnoreCase, Regex, Negate, First)

  override def call(target: MashValue, boundParams: BoundParams): MashValue = {
    val ignoreCase = boundParams(IgnoreCase).isTruthy
    val regex = boundParams(Regex).isTruthy
    val negate = boundParams(Negate).isTruthy
    val query = ToStringifier.stringify(boundParams(Query))
    val items = GrepFunction.getItems(target.asInstanceOf[MashString])
    val first = boundParams(First).isTruthy
    val options = GrepOptions(ignoreCase, regex, negate, first, ignoreFields = true)
    GrepFunction.runGrep(items, query, options)
  }

  override object typeInferenceStrategy extends MethodTypeInferenceStrategy {

    def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
      val argBindings = params.bindTypes(arguments)
      val first = argBindings.getBooleanValue(First).isDefined
      targetTypeOpt.map(_.when(!first, _.seq))
    }

  }

  override def summaryOpt: Option[String] = Some("Find all the elements in the lines of this String which match the given query")
}
