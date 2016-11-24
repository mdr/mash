package com.github.mdr.mash.ns.core

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.{ Arguments, MashClass }
import com.github.mdr.mash.functions.{ MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.parser.AbstractSyntax.StringLiteral
import com.github.mdr.mash.runtime._

import scala.collection.immutable.ListMap

object ObjectClass extends MashClass("core.Object") {

  override val methods = Seq(
    FieldsMethod,
    GetMethod,
    HasFieldMethod,
    HoistMethod,
    WithFieldMethod)

  object HoistMethod extends MashMethod("hoist") {

    object Params {
      val FieldName = Parameter(
        name = "fieldName",
        summary = "Field name to hoist")
    }
    import Params._

    val params = ParameterModel(Seq(FieldName))

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      val obj = target.asInstanceOf[MashObject]
      val boundParams = params.validate(arguments)
      val field = boundParams.validateString(FieldName).s
      val fieldValue = obj.get(field).getOrElse(boundParams.throwInvalidArgument(FieldName, "No field named " + field))
      fieldValue match {
        case subObject: MashObject ⇒
          hoist(obj, field, subObject)
        case xs: MashList =>
          MashList(xs.items.map {
            case subObject: MashObject => hoist(obj, field, subObject)
            case x => boundParams.throwInvalidArgument(FieldName, "Field value is not an object, but instead a " + x.typeName)
          })
        case x             ⇒ boundParams.throwInvalidArgument(FieldName, "Field value is not an object, but instead a " + x.typeName)
      }
    }

    private def hoist(obj: MashObject, field: String, subObject: MashObject): MashObject = {
      val subFields = subObject.fields.toSeq
      val originalFields = obj.fields.toSeq
      val index = originalFields.indexWhere(_._1 == field)
      val newFields = originalFields.take(index) ++ subFields ++ originalFields.drop(index + 1)
      MashObject.of(newFields)
    }

    override def typeInferenceStrategy = HoistMethodTypeInferenceStrategy

    object HoistMethodTypeInferenceStrategy extends MethodTypeInferenceStrategy {

      def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
        val argBindings = HoistMethod.params.bindTypes(arguments)
        targetTypeOpt.flatMap {
          case Type.Instance(klass) if klass isSubClassOf ObjectClass ⇒
            Some(Type.Instance(klass))
          case Type.Object(fields) ⇒
            for {
              AnnotatedExpr(nameExprOpt, _) ← argBindings.get(HoistMethod.Params.FieldName)
              StringLiteral(fieldName, _, _, _) ← nameExprOpt
              fieldType <- fields.get(fieldName)
              (newFields, isList) <- PartialFunction.condOpt(fieldType) {
                case Type.Object(newFields) => (newFields, false)
                case Type.Seq(Type.Object(newFields)) => (newFields, true)
              }
              newObjectType = Type.Object((fields - fieldName) ++ newFields)
            } yield if (isList) Type.Seq(newObjectType) else newObjectType
          case _ ⇒
            None
        }
      }

    }

    override def summary = "Hoist the fields of a subobject up into this object"

    override def descriptionOpt = Some("""Examples:
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

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Seq(FieldAndValueClass))

    override def summary = "Return the fields of this object"
  }

  object HasFieldMethod extends MashMethod("hasField") {

    object Params {
      val Name = Parameter(
        name = "name",
        summary = "Field name")
    }
    import Params._

    val params = ParameterModel(Seq(Name))

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      val obj = target.asInstanceOf[MashObject]
      val boundParams = params.validate(arguments)
      val field = boundParams.validateString(Name).s
      MashBoolean(obj.fields.contains(field))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Return true if this object contains the given field"
  }

  object GetMethod extends MashMethod("get") {

    object Params {
      val Name = Parameter(
        name = "name",
        summary = "Field name")
      val Default = Parameter(
        name = "default",
        summary = "Default to use if no field with that name present in object (default null)",
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
            AnnotatedExpr(nameExprOpt, _) ← argBindings.get(Name)
            StringLiteral(fieldName, _, _, _) ← nameExprOpt
            targetType ← targetTypeOpt
            fieldType ← targetType match {
              case Type.Object(fields)  ⇒ fields.get(fieldName)
              case Type.Instance(klass) ⇒ klass.fieldsMap.get(fieldName).map(_.fieldType)
              case _                    ⇒ None
            }
          } yield fieldType
        fieldTypeOpt orElse argBindings.get(Default).flatMap(_.typeOpt)
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

    private def getFields(typ_ : Type): Seq[String] = typ_ match {
      case Type.Object(fields)  ⇒ fields.keys.toSeq
      case Type.Instance(klass) ⇒ klass.fields.map(_.name)
      case _                    ⇒ Seq()
    }

    override def summary = "Get the value of the given field, else use a default (null by default)"
  }

  object WithFieldMethod extends MashMethod("withField") {

    object Params {
      val Name = Parameter(
        name = "name",
        summary = "Field name")
      val Value = Parameter(
        name = "value",
        summary = "Field value")
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
            Some(Type.Instance(klass))
          case Type.Object(fields) ⇒
            for {
              AnnotatedExpr(nameExprOpt, _) ← argBindings.get(WithFieldMethod.Params.Name)
              StringLiteral(s, _, _, _) ← nameExprOpt
              AnnotatedExpr(_, valueTypeOpt) ← argBindings.get(WithFieldMethod.Params.Value)
              valueType ← valueTypeOpt
            } yield Type.Object(fields + (s -> valueType))
          case _ ⇒
            None
        }
      }

    }

    override def typeInferenceStrategy = WithFieldMethodTypeInferenceStrategy

    override def summary = "Return a copy of this object with the given field added or updated with the given value."
  }

  override def summary = "The class of all objects"

  override def parentOpt = Some(AnyClass)

}