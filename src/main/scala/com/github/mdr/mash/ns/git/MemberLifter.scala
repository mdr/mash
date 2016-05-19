package com.github.mdr.mash.ns.git

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy

class MemberLifter(getFullObject: MashString ⇒ MashObject) {

  def liftField(field: Field) = new MashMethod(field.name) {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): Any = {
      params.validate(arguments)
      val hash = target.asInstanceOf[MashString]
      val obj = getFullObject(hash)
      obj.fields(field.name)
    }

    override def typeInferenceStrategy =
      ConstantMethodTypeInferenceStrategy(field.fieldType)

    override def summary = field.summary

    override def descriptionOpt = field.descriptionOpt

  }

  def liftMethod(method: MashMethod) = new MashMethod(method.name) {

    val params = method.params

    def apply(target: Any, arguments: Arguments): Any = {
      val hash = target.asInstanceOf[MashString]
      val obj = getFullObject(hash)
      method.apply(obj, arguments)
    }

    override def typeInferenceStrategy = method.typeInferenceStrategy
    override def summary = method.summary
    override def descriptionOpt = method.descriptionOpt

  }

}