package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.runtime.{ MashList, MashNumber, MashObject, MashValue }

object GroupClass extends MashClass("collections.Group") {

  object Fields {
    val Key = Field("key", "Key that each element of the group shares", Type.Instance(StringClass))
    val Values = Field("values", "Values of the group", Type.Seq(Type.Any))
  }
  import Fields._

  override val fields = Seq(Key, Values)

  override val methods = Seq(CountMethod)

  private case class Wrapper(target: MashValue) {

    private val obj = target.asInstanceOf[MashObject]

    def values = obj(Values).asInstanceOf[MashList]

  }

  object CountMethod extends MashMethod("count") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      MashNumber(Wrapper(target).values.length)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(NumberClass)

    def summary = "The number of values in this group"

  }

  override def summary = "A group of values with a common key, as produced by the groupBy function"

}