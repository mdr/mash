package com.github.mdr.mash.inference

import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime.{ MashString, MashValue }

/**
 * Inferencer used by type-inference strategies
 */
class Inferencer(typeInferencer: TypeInferencer, bindings: Map[String, Type]) {

  def applyFunction(functionType: Type, elementType: Type, functionExprValueOpt: Option[MashValue]): Option[Type] = functionType match {
    case Type.BuiltinFunction(f)                                  ⇒
      val strategy = f.typeInferenceStrategy
      val args = Seq(positionArg(elementType))
      strategy.inferTypes(this, TypedArguments(args))
    case Type.BoundMethod(targetType, method)                     ⇒
      val strategy = method.typeInferenceStrategy
      val args = Seq(positionArg(elementType))
      strategy.inferTypes(this, Some(targetType), TypedArguments(args))
    case Type.Function(parameterModel, expr, lambdaBindings)      ⇒
      parameterModel.params.headOption.flatMap { param ⇒
        typeInferencer.inferType(expr, lambdaBindings ++ bindings + (param.name -> elementType))
      }
    case Type.Instance(StringClass) | Type.Tagged(StringClass, _) ⇒
      functionExprValueOpt.flatMap {
        case MashString(s, _) ⇒ typeInferencer.memberLookup(elementType, s, immediateExec = true)
        case _                ⇒ None
      }
    case _                                                        ⇒
      None
  }

  private def positionArg(typ: Type) = TypedArgument.PositionArg(ValueInfo(None, Some(typ)))

  def applyFunction2(functionType: Type, element1Type: Type, element2Type: Type): Option[Type] = functionType match {
    case Type.BuiltinFunction(f)                             ⇒
      val strategy = f.typeInferenceStrategy
      val args = Seq(positionArg(element1Type), positionArg(element2Type))
      strategy.inferTypes(this, TypedArguments(args))
    case Type.BoundMethod(targetType, method)                ⇒
      val strategy = method.typeInferenceStrategy
      val args = Seq(positionArg(element1Type), positionArg(element2Type))
      strategy.inferTypes(this, Some(targetType), TypedArguments(args))
    case Type.Function(parameterModel, expr, lambdaBindings) ⇒
      val paramBindings = parameterModel.params.map(_.name).zip(Seq(element1Type, element2Type)).toMap
      typeInferencer.inferType(expr, lambdaBindings ++ bindings ++ paramBindings)
    case _                                                   ⇒
      None
  }

  def memberLookup(typ: Type, name: String): Option[Type] = typeInferencer.memberLookup(typ, name, immediateExec = true)

}
