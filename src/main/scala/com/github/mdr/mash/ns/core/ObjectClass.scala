package com.github.mdr.mash.ns.core

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.{ Arguments, MashClass }
import com.github.mdr.mash.functions.{ MashFunction, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime._

import scala.PartialFunction.condOpt
import scala.collection.immutable.ListMap

object ObjectClass extends MashClass("core.Object") {

  override val staticMethods = Seq(
    MergeFunction)

  override val methods = Seq(
    FieldsMethod,
    GetMethod,
    HasFieldMethod,
    HoistMethod,
    WithFieldMethod)

  object MergeFunction extends MashFunction("merge") {

    object Params {
      val Objects = Parameter(
        nameOpt = Some("objects"),
        summaryOpt = Some("Objects to merge"),
        isVariadic = true,
        variadicAtLeastOne = true)
    }

    import Params._

    val params = ParameterModel(Seq(Objects))

    override def apply(arguments: Arguments): MashObject = {
      val boundParams = params.validate(arguments)
      val xs = boundParams.validateSequence(Objects)
      val items = xs match {
        case Seq(list: MashList) ⇒ list.elements
        case _                   ⇒ xs
      }
      val objects = items.map {
        case item: MashObject ⇒ item
        case badItem          ⇒ boundParams.throwInvalidArgument(Objects, "Cannot merge value of type " + badItem.typeName)
      }
      objects.reduceOption(_ + _) getOrElse MashObject.empty
    }

    override def summaryOpt = Some("Merge objects together")

  }

  object HoistMethod extends MashMethod("hoist") {

    object Params {
      val FieldName = Parameter(
        nameOpt = Some("fieldName"),
        summaryOpt = Some("Field name to hoist"))
      val Prefix = Parameter(
        nameOpt = Some("prefix"),
        summaryOpt = Some("Add this prefix to hoisted field names"),
        defaultValueGeneratorOpt = Some(() ⇒ MashNull),
        isFlag = true,
        flagValueNameOpt = Some("prefix"))
    }

    import Params._

    val params = ParameterModel(Seq(FieldName, Prefix))

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      val obj = target.asInstanceOf[MashObject]
      val boundParams = params.validate(arguments)
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

  }

  object FieldsMethod extends MashMethod("fields") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = {
      val obj = target.asInstanceOf[MashObject]
      def asObject(name: String, value: MashValue) = {
        import FieldAndValueClass.Fields._
        MashObject.of(ListMap(
          Name -> MashString(name),
          Value -> value), FieldAndValueClass)
      }
      MashList(obj.fields.toSeq.map((asObject _).tupled))
    }

    override def typeInferenceStrategy = Seq(FieldAndValueClass)

    override def summaryOpt = Some("Return the fields of this object")
  }

  object HasFieldMethod extends MashMethod("hasField") {

    object Params {
      val Name = Parameter(
        nameOpt = Some("name"),
        summaryOpt = Some("Field name"))
    }

    import Params._

    val params = ParameterModel(Seq(Name))

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      val obj = target.asInstanceOf[MashObject]
      val boundParams = params.validate(arguments)
      val field = boundParams.validateString(Name).s
      MashBoolean(obj.fields.contains(field))
    }

    override def typeInferenceStrategy = BooleanClass

    override def summaryOpt = Some("Return true if this object contains the given field")
  }

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

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      val obj = target.asInstanceOf[MashObject]
      val boundParams = params.validate(arguments)
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

    val params = ParameterModel(Seq(Name, Value))

    def apply(target: MashValue, arguments: Arguments): MashObject = {
      val obj = target.asInstanceOf[MashObject]
      val boundParams = params.validate(arguments)
      val field = boundParams.validateString(Name).s
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
  }

  override def summaryOpt = Some("The class of all objects")

  override def parentOpt = Some(AnyClass)

}