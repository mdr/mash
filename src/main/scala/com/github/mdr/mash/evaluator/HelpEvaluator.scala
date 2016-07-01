package com.github.mdr.mash.evaluator

import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.ns.core.help.HelpFunction
import scala.PartialFunction.condOpt

object HelpEvaluator {

  def evaluateHelpExpr(helpExpr: HelpExpr)(implicit context: EvaluationContext): MashObject =
    helpExpr.expr match {
      case memberExpr @ MemberExpr(targetExpr, name, _, _) ⇒
        val target = Evaluator.evaluate(targetExpr)
        val scalarHelpOpt = getHelpForMember(target, name)
        lazy val vectorHelpOpt = condOpt(target) {
          case MashList(x, _*) ⇒ getHelpForMember(x, name)
        }.flatten
        lazy val directHelp = {
          val result = MemberEvaluator.evaluateMemberExpr_(memberExpr, target, immediatelyResolveNullaryWhenVectorising = true).result
          HelpFunction.getHelp(result).getOrElse(
            throw new EvaluatorException("No help available for value of type " + result.primaryClass, helpExpr.locationOpt))
        }
        scalarHelpOpt orElse vectorHelpOpt getOrElse directHelp
      case expr ⇒
        val result = Evaluator.simpleEvaluate(expr)
        HelpFunction.getHelp(result).getOrElse(
          throw new EvaluatorException("No help available for value of type " + result.primaryClass, helpExpr.locationOpt))
    }

  private def lookupField(target: MashValue, name: String): Option[(Field, MashClass)] =
    condOpt(target) {
      case MashObject(_, Some(klass)) ⇒ klass.fields.find(_.name == name).map(field ⇒ (field, klass))
    }.flatten

  private def getHelpForMember(target: MashValue, name: String): Option[MashObject] = {
    val fieldHelpOpt = lookupField(target, name).map { case (field, klass) ⇒ HelpFunction.getHelp(field, klass) }
    lazy val memberHelpOpt = MemberEvaluator.maybeLookup(target, name).collect {
      case method: BoundMethod ⇒ HelpFunction.getHelp(method)
    }
    fieldHelpOpt orElse memberHelpOpt
  }

}