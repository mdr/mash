package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions.ArgumentException
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.BinaryOperator
import com.github.mdr.mash.runtime._

object AssignmentEvaluator extends EvaluatorHelper {

  def evaluateAssignment(expr: AssignmentExpr)(implicit context: EvaluationContext): MashValue = {
    val AssignmentExpr(left, operatorOpt, right, alias, _) = expr
    val rightValue = if (alias) Evaluator.simpleEvaluate(right) else Evaluator.evaluate(right)

    left match {
      case identifier@Identifier(name, _)                         ⇒
        val actualRightValue = operatorOpt match {
          case Some(op) ⇒
            val currentValue = Evaluator.evaluateIdentifier(identifier)
            BinaryOperatorEvaluator.evaluateBinOp(currentValue, op, rightValue, sourceLocation(expr))
          case None     ⇒
            rightValue
        }
        context.scopeStack.set(name, actualRightValue)
        actualRightValue
      case memberExpr@MemberExpr(_, _, /* isNullSafe */ false, _) ⇒
        evaluateAssignmentToMemberExpr(memberExpr, expr, operatorOpt, rightValue)
      case lookupExpr: LookupExpr                                 ⇒
        evaluateAssignmentToLookupExpr(lookupExpr, expr, operatorOpt, rightValue)
      case _                                                      ⇒
        // TODO: this is purely syntactic, and therefore should be handled by the parser/compiler, not evaluator
        throw new EvaluatorException("Expression is not assignable", sourceLocation(left))
    }
  }

  def evaluatePatternAssignment(expr: PatternAssignmentExpr)(implicit context: EvaluationContext): MashValue = {
    val PatternAssignmentExpr(pattern, right, _) = expr
    val rightValue = Evaluator.evaluate(right)
    pattern match {
      case ObjectPattern(fieldNames, _) =>
        rightValue match {
          case obj: MashObject =>
            for (fieldName <- fieldNames)
              context.scopeStack.set(fieldName, obj.get(fieldName).getOrElse(MashNull))
          case _               =>
            throw new ArgumentException(s"Cannot match object pattern against value of type " + rightValue.typeName, sourceLocation(expr))
        }
      case HolePattern(_)               ⇒
    }
    rightValue
  }

  private def evaluateAssignmentToLookupExpr(lookupExpr: LookupExpr, expr: AssignmentExpr, operatorOpt: Option[BinaryOperator], rightValue: MashValue)(implicit context: EvaluationContext): MashValue = {
    val LookupExpr(target, index, _) = lookupExpr
    val targetValue = Evaluator.evaluate(target)
    val indexValue = Evaluator.evaluate(index)
    targetValue match {
      case xs: MashList    ⇒
        evaluateAssignmentToListIndex(lookupExpr, xs, index, indexValue, operatorOpt, rightValue)
      case obj: MashObject ⇒
        evaluateAssignmentToObject(lookupExpr, expr, obj, index, indexValue, operatorOpt, rightValue)
      case x               ⇒
        throw new EvaluatorException("Cannot assign to indexes of objects of type " + x.typeName, sourceLocation(target))
    }
  }

  private def evaluateAssignmentToListIndex(lookupExpr: LookupExpr, xs: MashList, index: Expr, indexValue: MashValue, operatorOpt: Option[BinaryOperator], rightValue: MashValue)(implicit context: EvaluationContext): MashValue =
    indexValue match {
      case n: MashNumber ⇒
        val i = n.asInt.getOrElse(
          throw new EvaluatorException("Invalid list index '" + indexValue + "'", sourceLocation(index)))
        if (i < 0 || i > xs.items.size - 1)
          throw new EvaluatorException("Index out of range '" + indexValue + "'", sourceLocation(index))
        val actualRightValue = operatorOpt match {
          case Some(op) ⇒
            val currentValue = xs.items(i)
            BinaryOperatorEvaluator.evaluateBinOp(currentValue, op, rightValue, sourceLocation(lookupExpr))
          case None     ⇒
            rightValue
        }
        xs.items(i) = actualRightValue
        actualRightValue
      case x             ⇒
        throw new EvaluatorException("Invalid list index of type " + x.typeName, sourceLocation(index))
    }

  private def evaluateAssignmentToObject(lookupExpr: LookupExpr,
                                         assignmentExpr: AssignmentExpr,
                                         obj: MashObject,
                                         index: Expr,
                                         indexValue: MashValue,
                                         operatorOpt: Option[BinaryOperator], rightValue: MashValue)(implicit context: EvaluationContext): MashValue = {
    val fields = obj.fields
    indexValue match {
      case MashString(fieldName, _) ⇒
        assignToField(obj, fieldName, operatorOpt, rightValue, lookupExpr, assignmentExpr)
      case _                        ⇒
        throw new EvaluatorException("Invalid object index of type " + indexValue.typeName, sourceLocation(index))
    }
  }

  private def evaluateAssignmentToMemberExpr(memberExpr: MemberExpr, assignmentExpr: AssignmentExpr, operatorOpt: Option[BinaryOperator], rightValue: MashValue)(implicit context: EvaluationContext): MashValue = {
    val MemberExpr(target, fieldName, _, _) = memberExpr
    Evaluator.evaluate(target) match {
      case obj: MashObject ⇒
        assignToField(obj, fieldName, operatorOpt, rightValue, memberExpr, assignmentExpr)
      case targetValue     ⇒
        throw new EvaluatorException("Cannot assign to fields of a value of type " + targetValue.typeName, sourceLocation(assignmentExpr))
    }
  }

  private def assignToField(obj: MashObject, fieldName: String, operatorOpt: Option[BinaryOperator], rightValue: MashValue, objectExpr: Expr, assignmentExpr: AssignmentExpr)(implicit context: EvaluationContext): MashValue = {
    val fields = obj.fields
    if (operatorOpt.isDefined && !fields.contains(fieldName))
      throw new EvaluatorException(s"No field '$fieldName' to update", sourceLocation(objectExpr))
    val actualRightValue = operatorOpt match {
      case Some(op) ⇒
        val currentValue = fields(fieldName)
        BinaryOperatorEvaluator.evaluateBinOp(currentValue, op, rightValue, sourceLocation(assignmentExpr))
      case None     ⇒
        rightValue
    }
    fields += fieldName -> actualRightValue
    actualRightValue
  }

}