package com.github.mdr.mash.evaluator
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime._

object AssignmentEvaluator {

  def evaluateAssignment(expr: AssignmentExpr)(implicit context: EvaluationContext): MashUnit = {
    val AssignmentExpr(left, right, alias, _) = expr
    val rightValue = if (alias) Evaluator.simpleEvaluate(right) else Evaluator.evaluate(right)
    left match {
      case Identifier(name, _) ⇒
        context.scopeStack.set(name, rightValue)
        MashUnit
      case MemberExpr(target, member, /* isNullSafe */ false, _) ⇒
        evaluateAssignmentToMemberExpr(expr, target, member, rightValue)
      case LookupExpr(target, index, _) ⇒
        evaluateAssignmentToLookupExpr(target, index, rightValue)
      case _ ⇒
        // TODO: this is purely syntactic, and therefore should be handled by the parser/compiler, not evaluator
        throw new EvaluatorException("Expression is not assignable", left.locationOpt)
    }
  }

  private def evaluateAssignmentToMemberExpr(expr: AssignmentExpr, target: Expr, member: String, rightValue: MashValue)(implicit context: EvaluationContext): MashUnit = {
    val targetValue = Evaluator.evaluate(target)
    targetValue match {
      case MashObject(fields, _) ⇒
        fields += member -> rightValue
        MashUnit
      case x ⇒
        throw new EvaluatorException("Cannot assign to fields of a value of type " + x.primaryClass, expr.locationOpt)
    }
  }

  private def evaluateAssignmentToLookupExpr(target: Expr, index: Expr, rightValue: MashValue)(implicit context: EvaluationContext): MashUnit = {
    val targetValue = Evaluator.evaluate(target)
    val indexValue = Evaluator.evaluate(index)
    targetValue match {
      case xs: MashList ⇒
        evaluateAssignmentToListIndex(xs, index, indexValue, rightValue)
      case mo: MashObject ⇒
        indexValue match {
          case MashString(s, _) ⇒ {
            mo.fields(s) = rightValue
            MashUnit
          }
          case x ⇒
            throw new EvaluatorException("Invalid object index of type " + x.primaryClass, index.locationOpt)
        }
      case x ⇒
        throw new EvaluatorException("Cannot assign to indexes of objects of type " + x.primaryClass, target.locationOpt)
    }
  }

  private def evaluateAssignmentToListIndex(xs: MashList, index: Expr, indexValue: MashValue, rightValue: MashValue): MashUnit =
    indexValue match {
      case n: MashNumber ⇒
        val i = n.asInt.getOrElse(
          throw new EvaluatorException("Invalid list index '" + indexValue + "'", index.locationOpt))
        val items = xs.items
        if (i < 0 || i > items.size - 1)
          throw new EvaluatorException("Index out of range '" + indexValue + "'", index.locationOpt)
        else {
          xs.items(i) = rightValue
          MashUnit
        }
      case x ⇒
        throw new EvaluatorException("Invalid list index of type " + x.primaryClass, index.locationOpt)
    }

  private def evaluateAssignmentToObject(obj: MashObject, index: Expr, indexValue: MashValue, rightValue: MashValue): MashValue =
    indexValue match {
      case MashString(s, _) ⇒ {
        obj.fields(s) = rightValue
        MashUnit
      }
      case x ⇒
        throw new EvaluatorException("Invalid object index of type " + x.primaryClass, index.locationOpt)
    }
}