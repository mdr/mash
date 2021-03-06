package com.github.mdr.mash.classes

import com.github.mdr.mash.evaluator.{ EvaluationContext, Evaluator, SourceLocation }
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.parser.AbstractSyntax.{ Expr, FunctionDeclaration, ParamList }
import com.github.mdr.mash.parser.DocComment
import com.github.mdr.mash.runtime.MashValue

case class UserDefinedMethod(docCommentOpt: Option[DocComment],
                             methodName: String,
                             params: ParameterModel,
                             paramList: ParamList,
                             body: Expr,
                             context: EvaluationContext,
                             override val isPrivate: Boolean,
                             override val aliases: Seq[String],
                             decl: FunctionDeclaration) extends MashMethod(methodName) {

  override def paramContext(target: MashValue) =
    context.copy(scopeStack = context.scopeStack.withFullScope(Map(), Set(), target))

  def call(target: MashValue, boundParams: BoundParams): MashValue = {
    val newScopeStack = context.scopeStack.withFullScope(boundParams.boundNames, boundParams.safeNames, target)
    val methodBodyEvalContext = context.copy(scopeStack = newScopeStack)
    Evaluator.evaluate(body)(methodBodyEvalContext)
  }

  override def summaryOpt = docCommentOpt.map(_.summary)

  override def descriptionOpt = docCommentOpt.flatMap(_.descriptionOpt)

  private def sourceLocationOpt: Option[SourceLocation] = decl.sourceInfoOpt.flatMap(_.locationOpt)

  override def sourceOpt: Option[String] = sourceLocationOpt.map(_.reindentedSource)

}