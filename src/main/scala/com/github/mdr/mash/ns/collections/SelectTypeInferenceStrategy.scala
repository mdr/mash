package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.utils.Utils

object SelectTypeInferenceStrategy extends TypeInferenceStrategy {

  import SelectFunction.Params._

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] =
    for {
      selectArgs ← Utils.initOpt(arguments.arguments)
      TypedArgument.PositionArg(ValueInfo(_, inputTypeOpt)) ← arguments.arguments.lastOption
      inputType ← inputTypeOpt
      targetType = inputType match {
        case Type.Seq(elementType) ⇒ elementType
        case _                     ⇒ inputType
      }
      fieldsAndTypes = selectArgs.flatMap(getFieldName(inferencer, targetType, _))
      add = arguments.argSet.contains(Add.name)
    } yield handle(inputType, fieldsAndTypes, inferencer, add = add)

  private def isSpecialFlag(flag: String) = flag == Add.name || flag == Target.name

  private def getFieldName(inferencer: Inferencer, elementType: Type, typedArg: TypedArgument): Option[(String, Option[Type])] =
    typedArg match {
      case TypedArgument.PositionArg(ValueInfo(functionValueOpt, Some(functionType)))                                  ⇒
        val typeOpt = inferencer.applyFunction(functionType, elementType, functionValueOpt)
        functionValueOpt.collect { case MashString(s, _) => s -> typeOpt }
      case TypedArgument.LongFlag(flag, Some(ValueInfo(functionValueOpt, Some(functionType)))) if !isSpecialFlag(flag) ⇒
        val typeOpt = inferencer.applyFunction(functionType, elementType, functionValueOpt)
        Some(flag -> typeOpt)
      case TypedArgument.LongFlag(flag, None) if !isSpecialFlag(flag)                                                  ⇒
        val typeOpt = inferencer.applyFunction(StringClass, elementType, Some(MashString(flag)))
        Some(flag -> typeOpt)
      case _                                                                                                           =>
        None
    }

  private def handle(inputType: Type, fieldsAndTypes: Seq[(String, Option[Type])], inferencer: Inferencer, add: Boolean): Type = {
    def newType(typ: Type) = {
      val baseFields: Map[String, Type] =
        if (add)
          inputType match {
            case Type.Object(fields)  ⇒ fields
            case Type.Instance(klass) ⇒ Map(klass.fieldsMap.mapValues(_.fieldType).toSeq: _*)
            case _                    ⇒ Map()
          }
        else
          Map()
      val fieldPairs =
        for {
          (field, fieldTypeOpt) ← fieldsAndTypes
          fieldType = fieldTypeOpt getOrElse Type.Any
        } yield field -> fieldType
      Type.Object(baseFields ++ fieldPairs.toMap)
    }
    inputType match {
      case Type.Seq(elementType) ⇒ Type.Seq(newType(elementType))
      case _                     ⇒ newType(inputType)
    }
  }

}