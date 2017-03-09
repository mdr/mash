package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.{ EvaluationContext, Evaluator }
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.parser.DocComment
import com.github.mdr.mash.runtime.MashValue

case class UserDefinedFunction(docCommentOpt: Option[DocComment],
                               functionName: String,
                               params: ParameterModel,
                               body: Expr,
                               context: EvaluationContext)
  extends MashFunction(nameOpt = Some(functionName), namespaceOpt = context.namespaceOpt) {

  def apply(boundParams: BoundParams): MashValue = {
    val newScopeStack = context.scopeStack.withFullScope(boundParams.boundNames)
    Evaluator.evaluate(body)(context.copy(scopeStack = newScopeStack))
  }

  override def paramContext = context

  override def summaryOpt = docCommentOpt.map(_.summary)

  override def descriptionOpt = docCommentOpt.flatMap(_.descriptionOpt)

}