package com.github.mdr.mash.classes

import com.github.mdr.mash.evaluator.{ Arguments, EvaluationContext, Evaluator }
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.parser.AbstractSyntax.{ Expr, ParamList }
import com.github.mdr.mash.parser.DocComment
import com.github.mdr.mash.runtime.MashValue

case class UserDefinedMethod(docCommentOpt: Option[DocComment],
                             methodName: String,
                             params: ParameterModel,
                             paramList: ParamList,
                             body: Expr,
                             context: EvaluationContext,
                             override val isPrivate: Boolean,
                             override val aliases: Seq[String]) extends MashMethod(methodName) {

  override def apply(target: MashValue, arguments: Arguments): MashValue = {
    val parameterEvalContext = Some(context.copy(scopeStack = context.scopeStack.withFullScope(Map(), target)))
    val boundParams = Evaluator.parameterModel(paramList, parameterEvalContext).validate(arguments)
    val methodBodyEvalContext = context.copy(scopeStack = context.scopeStack.withFullScope(boundParams.boundNames, target))
    Evaluator.evaluate(body)(methodBodyEvalContext)
  }

  override def summaryOpt = docCommentOpt.map(_.summary)

  override def descriptionOpt = docCommentOpt.flatMap(_.descriptionOpt)

}