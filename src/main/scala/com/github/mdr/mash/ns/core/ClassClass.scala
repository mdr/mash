package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.ns.core.help.FunctionHelpClass
import com.github.mdr.mash.ns.core.help.HelpFunction
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.core.help.ClassHelpClass

object ClassClass extends MashClass("core.Class") {

  override val methods = Seq(
    FullNameMethod,
    HelpMethod,
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

  object HelpMethod extends MashMethod("help") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashObject = {
      params.validate(arguments)
      HelpFunction.getHelp(target.asInstanceOf[MashClass])
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(ClassHelpClass))

    override def summary = "Help documentation for this class"
  }

  override def summary = "A class"

}