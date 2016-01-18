package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.Arguments

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type

object ClassClass extends MashClass("core.Class") {

  override val methods = Seq(
    FullNameMethod,
    NameMethod,
    ParentMethod)

  object FullNameMethod extends MashMethod("fullName") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      MashString(target.asInstanceOf[MashClass].fullyQualifiedName)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(StringClass))

    override def summary = "Return the fully-qualified name of this class"

  }

  object NameMethod extends MashMethod("name") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      MashString(target.asInstanceOf[MashClass].name)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(StringClass))

    override def summary = "Return the name of this class"

  }
  object ParentMethod extends MashMethod("parent") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashClass = {
      params.validate(arguments)
      target.asInstanceOf[MashClass].parentOpt.orNull
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(ClassClass))

    override def summary = "Return the parent of this class, else null"

  }

  override def summary = "A class"

}