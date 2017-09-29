package com.github.mdr.mash.ns.git

import com.github.mdr.mash.classes.Field
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.runtime.{ MashObject, MashString, MashValue }

class MemberLifter(getFullObject: MashString ⇒ MashObject) {

  def liftField(field: Field) = new MashMethod(field.name) {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashValue = {
      val hash = target.asInstanceOf[MashString]
      val obj = getFullObject(hash)
      obj.get(field.name).getOrElse(throw EvaluatorException("No field found: " + field.name))
    }

    override def typeInferenceStrategy = field.fieldType

    override def summaryOpt = field.summaryOpt

    override def descriptionOpt = field.descriptionOpt

  }

  def liftMethod(method: MashMethod) = new MashMethod(method.name) {

    override def aliases = method.aliases

    val params = method.params

    override def call(target: MashValue, boundParams: BoundParams): MashValue = {
      val hash = target.asInstanceOf[MashString]
      val obj = getFullObject(hash)
      method.call(obj, boundParams)
    }

    override def typeInferenceStrategy = method.typeInferenceStrategy
    override def summaryOpt = method.summaryOpt
    override def descriptionOpt = method.descriptionOpt

  }

}