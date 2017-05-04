package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.NoArgFunction.NoArgValue
import com.github.mdr.mash.ns.core.{ NoArgFunction, ObjectClass }
import com.github.mdr.mash.runtime._

import scala.PartialFunction._

object HoistMethod extends MashMethod("hoist") {

  object Params {
    val FieldName = Parameter(
      nameOpt = Some("fieldName"),
      summaryOpt = Some("Field name to hoist"))
    val Prefix = Parameter(
      nameOpt = Some("prefix"),
      summaryOpt = Some("Add this prefix to hoisted field names"),
      defaultValueGeneratorOpt = Some(NoArgValue),
      isFlag = true,
      flagValueNameOpt = Some("prefix"))
  }

  import Params._

  val params = ParameterModel(Seq(FieldName, Prefix))

  def apply(target: MashValue, boundParams: BoundParams): MashValue = {
    val obj = target.asInstanceOf[MashObject]
    val field = boundParams.validateString(FieldName).s
    val prefixOpt = boundParams.validateStringOpt(Prefix).map(_.s)
    val fieldValue = obj.get(field).getOrElse(
      boundParams.throwInvalidArgument(FieldName, s"No '$field' field in value of type ${obj.typeName}"))
    fieldValue match {
      case subObject: MashObject ⇒
        hoist(obj, field, subObject, prefixOpt)
      case xs: MashList          ⇒
        MashList(xs.elements.map {
          case subObject: MashObject ⇒
            hoist(obj, field, subObject, prefixOpt)
          case x                     ⇒
            boundParams.throwInvalidArgument(FieldName, "Field value is not an object, but instead a " + x.typeName)
        })
      case x                     ⇒ boundParams.throwInvalidArgument(FieldName, "Field value is not an object, but instead a " + x.typeName)
    }
  }

  private def hoist(obj: MashObject, field: String, subObject: MashObject, prefixOpt: Option[String]): MashObject = {
    val subFields = subObject.fields.toSeq.map { case (subfield, value) ⇒ (prefixOpt.getOrElse("") + subfield) -> value }
    val originalFields = obj.fields.toSeq
    val index = originalFields.indexWhere(_._1 == field)
    val newFields = originalFields.take(index) ++ subFields ++ originalFields.drop(index + 1)
    MashObject.of(newFields)
  }

  override def typeInferenceStrategy = HoistMethodTypeInferenceStrategy

  object HoistMethodTypeInferenceStrategy extends MethodTypeInferenceStrategy {

    def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
      val argBindings = HoistMethod.params.bindTypes(arguments)
      for {
        ValueInfo(valueOpt, _) ← argBindings.getArgument(HoistMethod.Params.FieldName)
        fieldName ← valueOpt.collect { case MashString(s, _) ⇒ s }
        fields <- targetTypeOpt.flatMap(getFields)
        fieldType <- fields.get(fieldName)
        (newFieldsOpt, isList) = fieldType match {
          case Type.Seq(elementType) ⇒ (getFields(elementType), true)
          case _                     ⇒ (getFields(fieldType), false)
        }
        newFields <- newFieldsOpt
        newObjectType = Type.Object((fields - fieldName) ++ newFields)
      } yield if (isList) Type.Seq(newObjectType) else newObjectType
    }

    private def getFields(typ: Type): Option[Map[String, Type]] = condOpt(typ) {
      case Type.Instance(klass) if klass isSubClassOf ObjectClass ⇒
        klass.fieldsMap.mapValues(_.fieldType)
      case Type.Object(fields)                                    ⇒ fields
    }

  }

  override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) = {
    val completionSpecOpt =
      for {
        param ← params.bindTypes(arguments).paramAt(argPos)
        if param == FieldName
        targetType ← targetTypeOpt
      } yield CompletionSpec.Items(GetMethod.getFields(targetType))
    completionSpecOpt.toSeq
  }

  override def summaryOpt = Some("Hoist the fields of a subobject up into this object")

  override def descriptionOpt = Some(
    """Examples:
  {
    foo: 42,
    bar: {
      baz1: 100,
      baz2: 200
    }
  }.hoist 'bar'
  # becomes:
  {
    foo: 42,
    baz1: 100,
    baz2: 200
  }
    """)

  override val isShy = true

}
