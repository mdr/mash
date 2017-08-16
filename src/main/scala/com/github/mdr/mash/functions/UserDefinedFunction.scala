package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.{ EvaluationContext, Evaluator, SourceLocation }
import com.github.mdr.mash.parser.AbstractSyntax.{ Expr, FunctionDeclaration }
import com.github.mdr.mash.parser.DocComment
import com.github.mdr.mash.runtime.MashValue

import scala.PartialFunction.cond

case class UserDefinedFunction(docCommentOpt: Option[DocComment],
                               functionName: String,
                               params: ParameterModel,
                               body: Expr,
                               context: EvaluationContext,
                               decl: FunctionDeclaration)
  extends MashFunction(nameOpt = Some(functionName), namespaceOpt = context.namespaceOpt) {

  def call(boundParams: BoundParams): MashValue = {
    val newScopeStack = context.scopeStack.withFullScope(boundParams.boundNames, boundParams.safeNames)
    Evaluator.evaluate(body)(context.copy(scopeStack = newScopeStack))
  }

  override def paramContext = context

  override def summaryOpt = docCommentOpt.map(_.summary)

  override def descriptionOpt = docCommentOpt.flatMap(_.descriptionOpt)

  private def sourceLocationOpt: Option[SourceLocation] = decl.sourceInfoOpt.flatMap(_.locationOpt)

  override def sourceOpt: Option[String] = sourceLocationOpt.map(_.reindentedSource)

  override def equals(that: Any): Boolean = cond(that) { case anyRef: AnyRef ⇒ anyRef eq this }

  override def hashCode = System.identityHashCode(this)

}