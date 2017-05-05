package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ Inferencer, MethodTypeInferenceStrategy, Type, TypedArguments }
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

object UnblessMethod extends MashMethod("unbless") {

  val params = ParameterModel()

  override def call(target: MashValue, boundParams: BoundParams): MashObject = {
    val obj = target.asInstanceOf[MashObject]
    obj.withoutClass
  }

  override object typeInferenceStrategy extends MethodTypeInferenceStrategy {
    def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
      targetTypeOpt collect {
        case Type.Instance(klass)              ⇒
          Type.Object(klass.fields.map(f ⇒ f.name -> f.fieldType).toMap)
        case Type.UserClassInstance(userClass) ⇒
          val pairs =
            for {
              param ← userClass.params.params
              name ← param.nameOpt
            } yield name -> Type.Any
          Type.Object(pairs.toMap)
      }
    }
  }

  override def summaryOpt: Option[String] = Some("This object without any associated class")

  override val isShy = true

}
