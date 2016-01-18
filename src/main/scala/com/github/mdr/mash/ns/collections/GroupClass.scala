package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core._

object GroupClass extends MashClass("collections.Group") {

  object Fields {
    val Key = Field("key", "Key that each element of the group shares", Type.Instance(StringClass))
    val Values = Field("values", "Values of the group", Type.Seq(Type.Any))
  }

  import Fields._

  override val fields = Seq(Key, Values)

  override val methods = Seq(CountMethod)

  object CountMethod extends MashMethod("count") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): Any = {
      params.validate(arguments)
      MashNumber(target.asInstanceOf[MashObject].field(Values).asInstanceOf[Seq[_]].length)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(NumberClass))

    def summary = "The number of elements in the group"

  }

  override def summary = "A group of values with a common key, as produced by the groupBy function"

}