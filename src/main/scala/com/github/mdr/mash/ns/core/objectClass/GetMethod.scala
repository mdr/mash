package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.{ MashNull, MashObject, MashString, MashValue }

object GetMethod extends MashMethod("get") {

  object Params {
    val Name = Parameter(
      nameOpt = Some("name"),
      summaryOpt = Some("Field name"))
    val Default = Parameter(
      nameOpt = Some("default"),
      summaryOpt = Some("Default to use if no field with that name present in object (default null)"),
      defaultValueGeneratorOpt = Some(() ⇒ MashNull))
  }

  import Params._

  val params = ParameterModel(Seq(Name, Default))

  def apply(target: MashValue, boundParams: BoundParams): MashValue = {
    val obj = target.asInstanceOf[MashObject]
    val field = boundParams.validateString(Name).s
    val default = boundParams(Default)
    obj.fields.getOrElse(field, default)
  }

  object GetMethodTypeInferenceStrategy extends MethodTypeInferenceStrategy {

    def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
      val argBindings = params.bindTypes(arguments)
      val fieldTypeOpt =
        for {
          ValueInfo(valueOpt, _) ← argBindings.getArgument(Name)
          fieldName ← valueOpt.collect { case MashString(s, _) ⇒ s }
          targetType ← targetTypeOpt
          fieldType ← targetType match {
            case Type.Object(fields)  ⇒ fields.get(fieldName)
            case Type.Instance(klass) ⇒ klass.fieldsMap.get(fieldName).map(_.fieldType)
            case _                    ⇒ None
          }
        } yield fieldType
      fieldTypeOpt orElse argBindings.getType(Default)
    }
  }

  override def typeInferenceStrategy = GetMethodTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) = {
    val completionSpecOpt =
      for {
        param ← params.bindTypes(arguments).paramAt(argPos)
        if param == Name
        targetType ← targetTypeOpt
      } yield CompletionSpec.Items(getFields(targetType))
    completionSpecOpt.toSeq
  }

  def getFields(type_ : Type): Seq[String] = type_ match {
    case Type.Object(fields)  ⇒ fields.keys.toSeq
    case Type.Instance(klass) ⇒ klass.fields.map(_.name)
    case _                    ⇒ Seq()
  }

  override def summaryOpt = Some("Get the value of the given field, else use a default (null by default)")
}
