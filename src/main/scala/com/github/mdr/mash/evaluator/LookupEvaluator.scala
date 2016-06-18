package com.github.mdr.mash.evaluator

import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.ns.core.help.HelpFunction
import scala.PartialFunction.condOpt

object LookupEvaluator {

  def evaluateLookupExpr(lookupExpr: LookupExpr)(implicit context: EvaluationContext): MashValue = {
    val LookupExpr(targetExpr, indexExpr, _) = lookupExpr
    val target = Evaluator.evaluate(targetExpr)
    val index = Evaluator.evaluate(indexExpr)
    val targetLocationOpt = targetExpr.locationOpt
    val indexLocationOpt = indexExpr.locationOpt
    val lookupLocationOpt = lookupExpr.locationOpt
    index match {
      case MashString(memberName, _) ⇒ MemberEvaluator.lookup(target, memberName, indexLocationOpt)
      case n: MashNumber ⇒
        val i = n.asInt.getOrElse(throw new EvaluatorException("Unable to lookup, non-integer index: " + n, lookupLocationOpt))
        target match {
          case xs: MashList ⇒
            val index = if (i < 0) i + xs.size else i
            if (index >= xs.size)
              throw new EvaluatorException("Index out of range " + n, indexLocationOpt)
            xs(index)
          case s: MashString ⇒ s.lookup(i)
          case _             ⇒ throw new EvaluatorException("Unable to lookup", lookupLocationOpt)
        }
      case _ ⇒
        throw new EvaluatorException("Unable to lookup", indexLocationOpt)
    }
  }
}