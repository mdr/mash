package com.github.mdr.mash.inference

import com.github.mdr.mash.runtime.{ MashString, MashValue }

/**
  * Inferencer used by type-inference strategies
  */
class Inferencer(typeInferencer: TypeInferencer, bindings: Map[String, Type]) {

  def applyFunction(functionType: Type,
                    elementType: Type,
                    functionExprValueOpt: Option[MashValue]): Option[Type] = functionType match {
    case Type.BuiltinFunction(f)                                                 ⇒
      val strategy = f.typeInferenceStrategy
      val args = Seq(positionArg(elementType))
      strategy.inferTypes(this, TypedArguments(args))
    case Type.BoundBuiltinMethod(targetType, method)                             ⇒
      val strategy = method.typeInferenceStrategy
      val args = Seq(positionArg(elementType))
      strategy.inferTypes(this, Some(targetType), TypedArguments(args))
    case Type.UserDefinedFunction(_, _, _, parameterModel, expr, lambdaBindings) ⇒
      parameterModel.params.headOption.flatMap { param ⇒
        typeInferencer.inferType(expr, lambdaBindings ++ bindings ++ param.nameOpt.map(_ -> elementType))
      }
    case Type.Patterns.AnyString(_)                                              ⇒
      functionExprValueOpt.flatMap {
        case MashString(s, _) ⇒ typeInferencer.memberLookup(elementType, s, immediateExec = true)
        case _                ⇒ None
      }
    case _                                                                       ⇒
      None
  }

  private def positionArg(typ: Type) = TypedArgument.PositionArg(ValueInfo(None, Some(typ)))

  def applyFunction2(functionType: Type, element1Type: Type, element2Type: Type): Option[Type] = functionType match {
    case Type.BuiltinFunction(f)                                                 ⇒
      val strategy = f.typeInferenceStrategy
      val args = Seq(positionArg(element1Type), positionArg(element2Type))
      strategy.inferTypes(this, TypedArguments(args))
    case Type.BoundBuiltinMethod(targetType, method)                             ⇒
      val strategy = method.typeInferenceStrategy
      val args = Seq(positionArg(element1Type), positionArg(element2Type))
      strategy.inferTypes(this, Some(targetType), TypedArguments(args))
    case Type.UserDefinedFunction(_, _, _, parameterModel, expr, lambdaBindings) ⇒
      val binding1 =
        for {
          param ← parameterModel.params.headOption
          name ← param.nameOpt
        } yield name -> element1Type
      val binding2 =
        for {
          param ← parameterModel.params.lift(1)
          name ← param.nameOpt
        } yield name -> element2Type
      typeInferencer.inferType(expr, lambdaBindings ++ bindings ++ binding1 ++ binding2)
    case _                                                                       ⇒
      None
  }

  def memberLookup(typ: Type, name: String): Option[Type] = typeInferencer.memberLookup(typ, name, immediateExec = true)

}
