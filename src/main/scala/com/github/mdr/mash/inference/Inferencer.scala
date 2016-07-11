package com.github.mdr.mash.inference

import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.parser.AbstractSyntax._

/**
 * Inferencer interface used by type-inference strategies
 */
trait Inferencer {

  def applyFunction(functionType: Type, elementType: Type, functionExprOpt: Option[Expr]): Option[Type]

  def memberLookup(typ: Type, name: String): Option[Type]

}

case class InferencerImpl(typeInferencer: TypeInferencer, bindings: Map[String, Type]) extends Inferencer {

  def applyFunction(functionType: Type, elementType: Type, functionExprOpt: Option[Expr]): Option[Type] = functionType match {
    case Type.DefinedFunction(f) ⇒
      val strategy = f.typeInferenceStrategy
      val args = Seq(TypedArgument.PositionArg(AnnotatedExpr(None, Some(elementType))))
      strategy.inferTypes(this, SimpleTypedArguments(args))
    case Type.BoundMethod(targetType, method) ⇒
      val strategy = method.typeInferenceStrategy
      val args = Seq(TypedArgument.PositionArg(AnnotatedExpr(None, Some(elementType))))
      strategy.inferTypes(this, Some(targetType), SimpleTypedArguments(args))
    case Type.Lambda(parameters, expr, lambdaBindings) ⇒
      typeInferencer.inferType(expr, lambdaBindings ++ bindings + (parameters.head -> elementType))
    case Type.Instance(StringClass) | Type.Tagged(StringClass, _) ⇒
      functionExprOpt match {
        case Some(StringLiteral(s, _, _, _)) ⇒
          typeInferencer.memberLookup(elementType, s, immediateExec = true)
        case _ ⇒ None
      }
    case _ ⇒ None
  }

  def memberLookup(typ: Type, name: String): Option[Type] = typeInferencer.memberLookup(typ, name, immediateExec = true)

}
