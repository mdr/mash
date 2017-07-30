package com.github.mdr.mash.evaluator

import com.github.mdr.mash.classes.{ BoundMethod, Field, MashClass }
import com.github.mdr.mash.ns.core.help.HelpCreator
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime._

import scala.PartialFunction.condOpt

object HelpEvaluator {

  def evaluateHelpExpr(helpExpr: HelpExpr)(implicit context: EvaluationContext): MashValue =
    helpExpr.expr match {
      case memberExpr @ MemberExpr(targetExpr, name, _, _) ⇒
        val target = Evaluator.evaluate(targetExpr)
        val scalarHelpOpt = getHelpForMember(target, name)
        lazy val vectorHelpOpt = condOpt(target) {
          case MashList(x, _*) ⇒ getHelpForMember(x, name)
        }.flatten
        lazy val directHelp = {
          val result = MemberEvaluator.evaluateMemberExpr(memberExpr, target, thisTarget = false, invokeNullaryWhenVectorising = true).result
          HelpCreator.getHelp(result)
        }
        scalarHelpOpt orElse vectorHelpOpt getOrElse directHelp
      case expr ⇒
        val result = Evaluator.simpleEvaluate(expr)
        HelpCreator.getHelp(result)
    }

  private def lookupField(target: MashValue, name: String): Option[(Field, MashClass)] =
    condOpt(target) {
      case MashObject(_, Some(klass)) ⇒ klass.fields.find(_.name == name).map(field ⇒ (field, klass))
    }.flatten

  private def getHelpForMember(target: MashValue, name: String): Option[MashValue] = {
    val fieldHelpOpt = lookupField(target, name).map { case (field, klass) ⇒ HelpCreator.getFieldHelp(field, klass) }
    lazy val memberHelpOpt = MemberEvaluator.maybeLookupByString(target, name).collect {
      case method: BoundMethod ⇒ HelpCreator.getHelp(method)
    }
    fieldHelpOpt orElse memberHelpOpt
  }

}