package com.github.mdr.mash.ns.collections.listClass

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ Type, TypedArguments }
import com.github.mdr.mash.ns.collections.{ ListClass, SelectFunction }
import com.github.mdr.mash.runtime.MashValue

object SelectMethod extends MashMethod("select") {

  import SelectFunction.Params._

  override val params = ParameterModel(Add, Selectors)

  override def call(target: MashValue, boundParams: BoundParams): MashValue =
    SelectFunction.doSelect(target, boundParams)

  override def summaryOpt: Option[String] = SelectFunction.summaryOpt

  override def descriptionOpt: Option[String] = SelectFunction.descriptionOpt

  override def typeInferenceStrategy = ListClass.methodise(SelectFunction).typeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments): Seq[CompletionSpec] =
    ListClass.methodise(SelectFunction).getCompletionSpecs(argPos, targetTypeOpt, arguments)

}
