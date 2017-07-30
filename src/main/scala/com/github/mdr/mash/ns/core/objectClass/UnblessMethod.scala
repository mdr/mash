package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ Inferencer, MethodTypeInferenceStrategy, Type, TypedArguments }
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

object UnblessMethod extends MashMethod("unbless") {

  val params = ParameterModel.Empty

  override def call(target: MashValue, boundParams: BoundParams): MashObject = {
    val obj = target.asInstanceOf[MashObject]
    obj.withoutClass
  }

  override object typeInferenceStrategy extends MethodTypeInferenceStrategy {
    def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
      targetTypeOpt collect {
        case instance: Type.Instance          ⇒ instance.unbless
        case instance: Type.UserClassInstance ⇒ instance.unbless
      }
    }
  }

  override def summaryOpt: Option[String] = Some("This object without any associated class")

  override val isShy = true

}
