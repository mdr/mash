package com.github.mdr.mash.completions

import com.github.mdr.mash.evaluator.{ BoundMethod, ToStringifier }
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.parser.{ AbstractSyntax, ConcreteSyntax }
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.utils.Region

object BindingCompleter {

  def completeBindings(programText: String, identifierToken: Token, parser: CompletionParser): Option[CompletionResult] = {
    val completions =
      for {
        expr ← findIdentifierExpr(programText, identifierToken, parser).toSeq
        (name, type_) ← expr.typeBindings.toSeq
        if name startsWith identifierToken.text
        (completionType, descriptionOpt) = completionTypeAndDescription(type_)
      } yield Completion(name, typeOpt = Some(completionType), descriptionOpt = descriptionOpt)
    CompletionResult.of(completions, identifierToken.region)
  }

  private def findIdentifierExpr(programText: String, identifierToken: Token, parser: CompletionParser): Option[AbstractSyntax.Expr] =
    parser.parse(programText).find {
      case expr: Expr if expr.sourceInfoOpt.map(_.node) contains ConcreteSyntax.Identifier(identifierToken) ⇒ expr
    }

  def completeBindings(bindings: Map[String, MashValue], prefix: String, region: Region): Option[CompletionResult] = {
    val completions =
      for {
        (name, value) ← bindings.toSeq
        if name startsWith prefix
        (completionType, descriptionOpt) = completionTypeAndDescription(value)
      } yield Completion(name, typeOpt = Some(completionType), descriptionOpt = descriptionOpt)
    CompletionResult.of(completions, region)
  }

  private def completionTypeAndDescription(value: MashValue): (CompletionType, Option[String]) = value match {
    case mf: MashFunction ⇒ (CompletionType.Function, mf.summaryOpt)
    case bf: BoundMethod  ⇒ (CompletionType.Method, bf.method.summaryOpt)
    case x                ⇒ (CompletionType.Binding, Some(ToStringifier.safeStringify(x)))
  }

  private def completionTypeAndDescription(type_ : Type): (CompletionType, Option[String]) = type_ match {
    case Type.BoundBuiltinMethod(_, method) ⇒ (CompletionType.Method, method.summaryOpt)
    case Type.BuiltinFunction(fun)          ⇒ (CompletionType.Function, fun.summaryOpt)
    case x                                  ⇒ (CompletionType.Binding, None)
  }

}