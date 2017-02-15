package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.runtime.{ MashNumber, MashValue }

object GroupClass extends MashClass("collections.Group") {

  object Fields {
    val Key = Field("key", Some("Key that each element of the group shares"), StringClass)
    val Values = Field("values", Some("Values of the group"), Type.Any.seq)
  }
  import Fields._

  override val fields = Seq(Key, Values)

  override val methods = Seq(CountMethod)

  override val staticMethods = Seq(NewStaticMethod(this))

  private case class Wrapper(targetValue: MashValue) extends AbstractObjectWrapper(targetValue) {

    def values = getListField(Values)

  }

  object CountMethod extends MashMethod("count") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashNumber = {
      params.validate(arguments)
      MashNumber(Wrapper(target).values.length)
    }

    override def typeInferenceStrategy = NumberClass

    override def summaryOpt = Some("The number of values in this group")

  }

  override def summaryOpt = Some("A group of values with a common key, as produced by the groupBy function")

}