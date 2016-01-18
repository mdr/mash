package com.github.mdr.mash.inference

import scala.PartialFunction.condOpt
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.ns.collections._

/**
 * Type inference strategy for functions
 */
trait TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type]

}

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
    case Type.Lambda(parameter, expr, lambdaBindings) ⇒
      typeInferencer.inferType(expr, lambdaBindings ++ bindings + (parameter -> elementType))
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

object NoTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = None

}

case class ConstantTypeInferenceStrategy(typ: Type) extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] =
    Some(typ)

}

object SeqToSeqTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = MapFunction.params.bindTypes(arguments)
    for {
      AnnotatedExpr(_, sequenceTypeOpt) ← argBindings.get(MapFunction.Params.Sequence)
      sequenceType ← sequenceTypeOpt
    } yield sequenceType
  }

}