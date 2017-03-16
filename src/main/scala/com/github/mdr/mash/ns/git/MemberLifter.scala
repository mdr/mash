package com.github.mdr.mash.ns.git

import com.github.mdr.mash.classes.Field
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.runtime.{ MashObject, MashString, MashValue }

class MemberLifter(getFullObject: MashString ⇒ MashObject) {

  def liftField(field: Field) = new MashMethod(field.name) {

    val params = ParameterModel()

    def apply(target: MashValue, boundParams: BoundParams): MashValue = {
      val hash = target.asInstanceOf[MashString]
      val obj = getFullObject(hash)
      obj.fields(field.name)
    }

    override def typeInferenceStrategy = field.fieldType

    override def summaryOpt = field.summaryOpt

    override def descriptionOpt = field.descriptionOpt

  }

  def liftMethod(method: MashMethod) = new MashMethod(method.name) {

    override def aliases = method.aliases

    val params = method.params

    override def apply(target: MashValue, boundParams: BoundParams): MashValue = {
      val hash = target.asInstanceOf[MashString]
      val obj = getFullObject(hash)
      method.apply(obj, boundParams)
    }

    override def typeInferenceStrategy = method.typeInferenceStrategy
    override def summaryOpt = method.summaryOpt
    override def descriptionOpt = method.descriptionOpt

  }

}