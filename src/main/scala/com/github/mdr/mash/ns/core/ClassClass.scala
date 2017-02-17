package com.github.mdr.mash.ns.core

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.help.{ ClassHelpClass, HelpCreator }
import com.github.mdr.mash.runtime.{ MashNull, MashObject, MashString, MashValue }

object ClassClass extends MashClass("core.Class") {

  override val methods = Seq(
    FullNameMethod,
    HelpMethod,
    NameMethod,
    ParentMethod)

  object FullNameMethod extends MashMethod("fullName") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      MashString(target.asInstanceOf[MashClass].fullyQualifiedName.toString)
    }

    override def typeInferenceStrategy = StringClass

    override def summaryOpt = Some("The fully-qualified name of this class")

  }

  object NameMethod extends MashMethod("name") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      MashString(target.asInstanceOf[MashClass].name)
    }

    override def typeInferenceStrategy = StringClass

    override def summaryOpt = Some("The name of this class")

  }

  object ParentMethod extends MashMethod("parent") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      target.asInstanceOf[MashClass].parentOpt.getOrElse(MashNull)
    }

    override def typeInferenceStrategy = ClassClass

    override def summaryOpt = Some("The parent of this class, if any, else null")

  }

  object HelpMethod extends MashMethod("help") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashObject = {
      params.validate(arguments)
      HelpCreator.getHelp(target.asInstanceOf[MashClass])
    }

    override def typeInferenceStrategy = ClassHelpClass

    override def summaryOpt = Some("Help documentation for this class")
  }

  override def summaryOpt = Some("A class")

  override def parentOpt = Some(AnyClass)

}