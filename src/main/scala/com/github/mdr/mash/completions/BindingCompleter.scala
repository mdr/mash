package com.github.mdr.mash.completions

import com.github.mdr.mash.evaluator.{ BoundMethod, ToStringifier }
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.parser.AbstractSyntax.{ Expr, Identifier, StringLiteral }
import com.github.mdr.mash.parser.{ ConcreteSyntax, SourceInfo }
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.utils.Region

object BindingCompleter {

  def completeBindings(text: String, identifierToken: Token, parser: CompletionParser): Option[CompletionResult] = {
    val exprOpt = parser.parse(text).find {
      case expr: Expr if expr.sourceInfoOpt.map(_.expr) contains ConcreteSyntax.Identifier(identifierToken) ⇒ expr
    }
    val completions =
      for {
        expr ← exprOpt.toSeq
        (name, type_) ← expr.typeBindings.toSeq
        if name startsWith identifierToken.text
        (completionType, descriptionOpt) = type_ match {
          case Type.BoundMethod(_, method) ⇒ (CompletionType.Method, Some(method.summary))
          case Type.BuiltinFunction(fun)   ⇒ (CompletionType.Function, Some(fun.summary))
          case x                           ⇒ (CompletionType.Binding, None)
        }
      } yield Completion(name, typeOpt = Some(completionType), descriptionOpt = descriptionOpt)
    CompletionResult.of(completions, identifierToken.region)
  }

  def completeBindings(bindings: Map[String, MashValue], prefix: String, region: Region): Option[CompletionResult] = {
    val completions =
      for {
        (name, value) ← bindings.toSeq
        if name startsWith prefix
        (completionType, description) = getBindingTypeAndDescription(value)
      } yield Completion(name, typeOpt = Some(completionType), descriptionOpt = Some(description))
    CompletionResult.of(completions, region)
  }

  private def getBindingTypeAndDescription(value: MashValue): (CompletionType, String) = value match {
    case mf: MashFunction ⇒ (CompletionType.Function, mf.summary)
    case bf: BoundMethod  ⇒ (CompletionType.Method, bf.method.summary)
    case x                ⇒ (CompletionType.Binding, ToStringifier.safeStringify(x))
  }

}