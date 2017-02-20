package com.github.mdr.mash.ns.core

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.evaluator.{ Arguments, EvaluatorException }
import com.github.mdr.mash.functions.{ MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ Inferencer, MethodTypeInferenceStrategy, Type, TypedArguments }
import com.github.mdr.mash.ns.core.help.{ ClassHelpClass, HelpCreator }
import com.github.mdr.mash.runtime.{ MashNull, MashObject, MashString, MashValue }

object ClassClass extends MashClass("core.Class") {

  override val methods = Seq(
    BlessMethod,
    FullNameMethod,
    HelpMethod,
    NameMethod,
    ParentMethod)

  object BlessMethod extends MashMethod("bless") {

    object Params {
      val Object = Parameter(
        nameOpt = Some("object"),
        summaryOpt = Some("Object to give this class"))
    }

    import Params._

    val params = ParameterModel(Seq(Object))

    override def apply(target: MashValue, arguments: Arguments): MashObject = {
      val boundParams = params.validate(arguments)
      val obj = boundParams(Object) match {
        case obj: MashObject ⇒ obj
        case value           ⇒ boundParams.throwInvalidArgument(Object, s"Must be an object, but was a ${value.typeName}")
      }
      val klass = target.asInstanceOf[MashClass]
      if (!klass.isSubClassOf(ObjectClass))
        throw new EvaluatorException("Only object classes can be used to bless values")
      obj withClass klass
    }

    override object typeInferenceStrategy extends MethodTypeInferenceStrategy {
      def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
        val argBindings = params.bindTypes(arguments)
        argBindings.getType(Object)
        val userClassOpt = targetTypeOpt collect { case userClass: Type.UserClass ⇒ Type.UserClassInstance(userClass) }
        val builtinClassOpt = targetTypeOpt collect { case klass: MashClass ⇒ Type.Instance(klass) }
        userClassOpt orElse builtinClassOpt
      }
    }

    override def summaryOpt: Option[String] = Some("Give the given object this class")

  }

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
      HelpCreator.getHelp(target)
    }

    override def typeInferenceStrategy = ClassHelpClass

    override def summaryOpt = Some("Help documentation for this class")
  }

  override def summaryOpt = Some("A class")

  override def parentOpt = Some(AnyClass)

}