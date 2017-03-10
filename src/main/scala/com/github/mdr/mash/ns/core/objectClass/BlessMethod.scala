package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ Inferencer, MethodTypeInferenceStrategy, Type, TypedArguments }
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

object BlessMethod extends MashMethod("bless") {

  object Params {
    val Class = Parameter(
      nameOpt = Some("class"),
      summaryOpt = Some("Class to associate with this object"))
  }

  import Params._

  val params = ParameterModel(Seq(Class))

  override def apply(target: MashValue, boundParams: BoundParams): MashObject = {
    val obj = target.asInstanceOf[MashObject]
    val klass = boundParams(Class) match {
      case klass: MashClass ⇒ klass
      case value            ⇒ boundParams.throwInvalidArgument(Class, s"Must be a class, but was a ${value.typeName}")
    }
    obj.withClass(klass)
  }

  override object typeInferenceStrategy extends MethodTypeInferenceStrategy {
    def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
      val argBindings = params.bindTypes(arguments)
      val userClassOpt = argBindings.getType(Class) collect { case userClass: Type.UserClass ⇒ Type.UserClassInstance(userClass) }
      val builtinClassOpt = argBindings.getArgument(Class).flatMap(_.valueOpt) collect {
        case klass: MashClass ⇒ Type.Instance(klass)
      }
      userClassOpt orElse builtinClassOpt
    }
  }

  override def summaryOpt: Option[String] = Some("Give this object the given class")
}
