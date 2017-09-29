package com.github.mdr.mash.evaluator

import com.github.mdr.mash.parser.AbstractSyntax.{ LookupExpr, ThisExpr }
import com.github.mdr.mash.runtime._

object LookupEvaluator extends EvaluatorHelper {

  def evaluateLookupExpr(lookupExpr: LookupExpr)(implicit context: EvaluationContext): MashValue = {
    val LookupExpr(targetExpr, indexExpr, _) = lookupExpr
    val target = Evaluator.evaluate(targetExpr)
    val thisTarget = targetExpr.isInstanceOf[ThisExpr]
    val index = Evaluator.evaluate(indexExpr)
    index match {
      case MashString(memberName, _) ⇒
        MemberEvaluator.lookupByString(target, memberName, includePrivate = thisTarget, sourceLocation(indexExpr))
      case n: MashNumber             ⇒
        val i = n.asInt.getOrElse(throw EvaluatorException("Unable to lookup, non-integer index: " + n, sourceLocation(lookupExpr)))
        target match {
          case xs: MashList    ⇒
            val index = if (i < 0) i + xs.size else i
            if (index >= xs.size)
              throw EvaluatorException("Index out of range " + n, sourceLocation(indexExpr))
            xs(index)
          case s: MashString   ⇒
            s.lookup(i)
          case obj: MashObject ⇒
            MemberEvaluator.lookup(target, index, includePrivate = thisTarget, sourceLocation(indexExpr))
          case _               ⇒
            throw EvaluatorException("Unable to lookup in target of type " + target.typeName, sourceLocation(lookupExpr))
        }
      case _                         ⇒
        target match {
          case obj: MashObject ⇒
            MemberEvaluator.lookup(target, index, includePrivate = thisTarget, sourceLocation(indexExpr))
          case _               ⇒
            throw EvaluatorException("Unable to lookup index of type " + index.typeName, sourceLocation(indexExpr))
        }
    }
  }
}