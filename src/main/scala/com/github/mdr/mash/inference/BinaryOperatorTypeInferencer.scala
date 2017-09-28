package com.github.mdr.mash.inference

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.ns.core.{ BooleanClass, NumberClass, StringClass }
import com.github.mdr.mash.parser.AbstractSyntax.{ BinOpExpr, Expr }
import com.github.mdr.mash.parser.BinaryOperator
import com.github.mdr.mash.runtime.{ MashList, MashString }

import scala.PartialFunction._

object BinaryOperatorTypeInferencer {

  private object StringLike {
    def unapply(type_ : Type): Option[Type] = condOpt(type_) {
      case Type.Tagged(StringClass, _) | Type.Instance(StringClass) ⇒ type_
    }
  }

  private object NumberLike {
    def unapply(type_ : Type): Option[Option[MashClass]] = condOpt(type_) {
      case Type.Tagged(NumberClass, klass) ⇒ Some(klass)
      case Type.Instance(NumberClass)      ⇒ None
    }
  }

  private object ThingWithFields {
    def unapply(type_ : Type): Option[Map[String, Type]] = getObjectFields(type_)
  }

  private def getObjectFields(type_ : Type): Option[Map[String, Type]] = condOpt(type_) {
    case Type.Instance(klass)    ⇒ fields(klass)
    case Type.Object(fields)     ⇒ fields
    case Type.Generic(klass, _*) ⇒ fields(klass)
  }

  private def fields(klass: MashClass): Map[String, Type] =
    klass.fieldsMap.map { case (fieldName, field) ⇒ fieldName -> field.fieldType }

  def inferTypeAdd(leftTypeOpt: Option[Type], rightTypeOpt: Option[Type]): Option[Type] =
    (leftTypeOpt, rightTypeOpt) match {
      case (Some(Type.Seq(leftElementType)), Some(Type.Seq(_)))   ⇒
        if (leftElementType == Type.Any) rightTypeOpt else leftTypeOpt
      case (Some(StringLike(_)), _)                               ⇒ leftTypeOpt
      case (_, Some(StringLike(_)))                               ⇒ rightTypeOpt
      case (Some(NumberLike(_)), _)                               ⇒ leftTypeOpt
      case (_, Some(NumberLike(_)))                               ⇒ rightTypeOpt
      case (_, Some(Type.Instance(klass))) if klass.isObjectClass ⇒ rightTypeOpt
      case (_, Some(Type.UserClassInstance(_)))                   ⇒ rightTypeOpt
      case (Some(Type.Instance(klass)), _) if klass.isObjectClass ⇒ leftTypeOpt
      case (Some(Type.UserClassInstance(_)), _)                   ⇒ leftTypeOpt
      case _                                                      ⇒
        val objectAdditionTypeOpt =
          for {
            leftFields ← leftTypeOpt.flatMap(getObjectFields)
            rightFields ← rightTypeOpt.flatMap(getObjectFields)
          } yield Type.Object(leftFields ++ rightFields)
        objectAdditionTypeOpt orElse Some(Type.Instance(NumberClass))
    }
}

trait BinaryOperatorTypeInferencer {
  self: TypeInferencer ⇒

  import BinaryOperatorTypeInferencer._

  private def inferTypeMultiply(leftTypeOpt: Option[Type], rightTypeOpt: Option[Type]): Option[Type] =
    (leftTypeOpt, rightTypeOpt) match {
      case (Some(NumberLike(Some(klass))), Some(NumberLike(_)))     ⇒ leftTypeOpt
      case (Some(NumberLike(None)), Some(NumberLike(klassOpt)))     ⇒ rightTypeOpt
      case (Some(NumberLike(_)), Some(StringLike(_) | Type.Seq(_))) ⇒ rightTypeOpt
      case (Some(StringLike(_) | Type.Seq(_)), Some(NumberLike(_))) ⇒ leftTypeOpt
      case _                                                        ⇒ Some(Type.Instance(NumberClass))
    }

  private def inferTypeSubtract(leftTypeOpt: Option[Type], rightTypeOpt: Option[Type], right: Expr): Option[Type] =
    (leftTypeOpt, rightTypeOpt) match {
      case (Some(Type.Tagged(NumberClass, _)), _)                   ⇒ leftTypeOpt
      case (_, Some(Type.Tagged(NumberClass, _)))                   ⇒ rightTypeOpt
      case (Some(ThingWithFields(leftFields)), Some(StringLike(_))) ⇒
        right.constantValueOpt collect {
          case MashString(fieldName, _) ⇒ Type.Object(leftFields - fieldName)
        }
      case (Some(ThingWithFields(leftFields)), Some(Type.Seq(_)))   ⇒
        right.constantValueOpt collect {
          case xs: MashList ⇒
            val removedFields = xs.elements.collect { case MashString(s, _) ⇒ s }.toSet
            Type.Object(leftFields.filterKeys(k ⇒ !removedFields.contains(k)))
        }
      case (Some(Type.Seq(_)), _)                                   ⇒ leftTypeOpt
      case (_, Some(Type.Seq(_)))                                   ⇒ rightTypeOpt
      case _                                                        ⇒ Some(NumberClass)
    }

  protected def inferTypeBinOpExpr(leftTypeOpt: Option[Type], op: BinaryOperator, rightTypeOpt: Option[Type], right: Expr): Option[Type] = {
    import BinaryOperator._
    op match {
      case Plus                                                                             ⇒
        inferTypeAdd(leftTypeOpt, rightTypeOpt)
      case Multiply                                                                         ⇒
        inferTypeMultiply(leftTypeOpt, rightTypeOpt)
      case Minus                                                                            ⇒
        inferTypeSubtract(leftTypeOpt, rightTypeOpt, right)
      case Divide                                                                           ⇒
        (leftTypeOpt, rightTypeOpt) match {
          case (Some(Type.Tagged(NumberClass, _)), _) ⇒ leftTypeOpt
          case (_, Some(Type.Tagged(NumberClass, _))) ⇒ rightTypeOpt
          case _                                      ⇒ Some(NumberClass)
        }
      case Equals | NotEquals | LessThan | LessThanEquals | GreaterThan | GreaterThanEquals ⇒
        Some(BooleanClass)
      case Sequence                                                                         ⇒
        rightTypeOpt
      case And | Or                                                                         ⇒
        leftTypeOpt orElse rightTypeOpt
    }
  }

  protected def inferTypeBinOpExpr(binOpExpr: Expr, bindings: Map[String, Type]): Option[Type] = {
    val BinOpExpr(left, op, right, _) = binOpExpr
    val leftTypeOpt = inferType(left, bindings)
    val rightTypeOpt = inferType(right, bindings)
    inferTypeBinOpExpr(leftTypeOpt, op, rightTypeOpt, right)
  }

}
