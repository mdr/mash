package com.github.mdr.mash.ns.collections

import scala.PartialFunction.condOpt
import scala.collection.immutable.ListMap
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.utils.Utils
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.parser.AbstractSyntax
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.parser.QuotationType

object SelectTypeInferenceStrategy extends TypeInferenceStrategy {

  import SelectFunction.Params._

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] =
    for {
      selectArgs ← Utils.initOpt(arguments.arguments)
      TypedArgument.PositionArg(AnnotatedExpr(_, inputTypeOpt)) ← arguments.arguments.lastOption
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
    condOpt(typedArg) {
      case TypedArgument.PositionArg(AnnotatedExpr(functionExprOpt @ Some(StringLiteral(s, _, _, _)), Some(functionType))) ⇒
        val typeOpt = inferencer.applyFunction(functionType, elementType, functionExprOpt)
        s -> typeOpt
      case TypedArgument.LongFlag(flag, Some(AnnotatedExpr(functionExprOpt, Some(functionType)))) if !isSpecialFlag(flag) ⇒
        val typeOpt = inferencer.applyFunction(functionType, elementType, functionExprOpt)
        flag -> typeOpt
      case TypedArgument.LongFlag(flag, None) if !isSpecialFlag(flag) ⇒
        val dummyStringLiteral = StringLiteral(flag, QuotationType.Single, tildePrefix = false, sourceInfoOpt = None)
        val typeOpt = inferencer.applyFunction(Type.Instance(StringClass), elementType, Some(dummyStringLiteral))
        flag -> typeOpt
    }

  private def handle(inputType: Type, fieldsAndTypes: Seq[(String, Option[Type])], inferencer: Inferencer, add: Boolean): Type = {
    def newType(typ: Type) = {
      val baseFields: ListMap[String, Type] =
        if (add)
          inputType match {
            case Type.Object(fields)  ⇒ fields
            case Type.Instance(klass) ⇒ ListMap(klass.fieldsMap.mapValues(_.fieldType).toSeq: _*)
            case _                    ⇒ ListMap()
          }
        else
          ListMap()
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