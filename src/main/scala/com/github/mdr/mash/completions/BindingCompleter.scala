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

  def completeBindings(programText: String,
                       identifierToken: Token,
                       parser: CompletionParser,
                       bindings: Map[String, MashValue]): Option[CompletionResult] = {
    val completions =
      for {
        expr ← findIdentifierExpr(programText, identifierToken, parser).toSeq
        (name, completion) ← getTypeBindingCompletions(expr) ++ getBindingCompletions(bindings)
        if name startsWith identifierToken.text
      } yield completion
    CompletionResult.of(completions, identifierToken.region)
  }

  private def getTypeBindingCompletions(expr: Expr): Map[String, Completion] =
    for ((name, type_) ← expr.typeBindings)
      yield name -> getCompletion(name, type_)

  private def getBindingCompletions(bindings: Map[String, MashValue]): Map[String, Completion] =
    for ((name, value) ← bindings)
      yield name -> getCompletion(name, value)

  private def findIdentifierExpr(programText: String, identifierToken: Token, parser: CompletionParser): Option[AbstractSyntax.Expr] =
    parser.parse(programText).find {
      case expr: Expr if expr.sourceInfoOpt.map(_.node) contains ConcreteSyntax.Identifier(identifierToken) ⇒ expr
    }

  def completeBindings(bindings: Map[String, MashValue], prefix: String, region: Region): Option[CompletionResult] = {
    val completions =
      for {
        (name, completion) ← getBindingCompletions(bindings).toSeq
        if name startsWith prefix
      } yield completion
    CompletionResult.of(completions, region)
  }

  private def getCompletion(name: String, value: MashValue): Completion = value match {
    case mf: MashFunction ⇒
      Completion(name, typeOpt = Some(CompletionType.Function), descriptionOpt = mf.summaryOpt)
    case bf: BoundMethod  ⇒
      Completion(name, typeOpt = Some(CompletionType.Method), descriptionOpt = bf.method.summaryOpt)
    case _                ⇒
      Completion(name, typeOpt = Some(CompletionType.Binding), descriptionOpt = Some(ToStringifier.safeStringify(value)))
  }

  private def getCompletion(name: String, type_ : Type): Completion = type_ match {
    case Type.BoundBuiltinMethod(_, method) ⇒
      Completion(name, typeOpt = Some(CompletionType.Method), descriptionOpt = method.summaryOpt)
    case Type.BuiltinFunction(fun)          ⇒
      Completion(name, typeOpt = Some(CompletionType.Function), descriptionOpt = fun.summaryOpt)
    case _                                  ⇒
      Completion(name, typeOpt = Some(CompletionType.Binding))
  }

}