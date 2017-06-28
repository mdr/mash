package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.runtime.{ MashObject, MashString, MashValue }

object WithFieldMethod extends MashMethod("withField") {

  object Params {
    val Name = Parameter(
      nameOpt = Some("name"),
      summaryOpt = Some("Field name"))
    val Value = Parameter(
      nameOpt = Some("value"),
      summaryOpt = Some("Field value"))
  }

  import Params._

  val params = ParameterModel(Name, Value)

  def call(target: MashValue, boundParams: BoundParams): MashObject = {
    val obj = target.asInstanceOf[MashObject]
    val field = boundParams(Name)
    val value = boundParams(Value)
    obj.withField(field, value)
  }

  object WithFieldMethodTypeInferenceStrategy extends MethodTypeInferenceStrategy {

    def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
      val argBindings = WithFieldMethod.params.bindTypes(arguments)
      targetTypeOpt.flatMap {
        case Type.Instance(klass) if klass isSubClassOf ObjectClass ⇒
          Some(klass)
        case Type.Object(fields)                                    ⇒
          for {
            ValueInfo(valueOpt, _) ← argBindings.getArgument(WithFieldMethod.Params.Name)
            fieldName ← valueOpt.collect { case MashString(s, _) ⇒ s }
            valueType = argBindings.getType(WithFieldMethod.Params.Value) getOrElse Type.Any
          } yield Type.Object(fields + (fieldName -> valueType))
        case _                                                      ⇒
          None
      }
    }

  }

  override def typeInferenceStrategy = WithFieldMethodTypeInferenceStrategy

  override def summaryOpt = Some("Return a copy of this object with the given field added or updated with the given value.")

  override val isShy = true

}
