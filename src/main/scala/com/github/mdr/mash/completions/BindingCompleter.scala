package com.github.mdr.mash.completions

import com.github.mdr.mash.classes.{ BoundMethod, MashClass }
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.parser.{ AbstractSyntax, ConcreteSyntax }
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.utils.{ Region, StringUtils }

object BindingCompleter {

  /**
    * Complete bindings at the given identifier.
    */
  def completeBindings(programText: String,
                       identifierToken: Token,
                       parser: CompletionParser,
                       bindings: Map[String, MashValue],
                       filterToMatchIdentifierPrefix: Boolean = true): Option[CompletionResult] = {
    val completions =
      for {
        expr ← findIdentifierExpr(programText, identifierToken, parser).toSeq
        (name, completion) ← getTypeBindingCompletions(expr) ++ getBindingCompletions(bindings)
        if name.startsWith(identifierToken.text) || !filterToMatchIdentifierPrefix
      } yield completion
    CompletionResult.of(completions, identifierToken.region)
  }

  private def findIdentifierExpr(programText: String, identifierToken: Token, parser: CompletionParser): Option[AbstractSyntax.Expr] =
    parser.parse(programText).find {
      case expr: Expr if expr.sourceInfoOpt.map(_.node) contains ConcreteSyntax.Identifier(identifierToken) ⇒ expr
    }

  /**
    * Complete bindings based on what's known by the type inferencer at that position
    */
  private def getTypeBindingCompletions(expr: Expr): Map[String, Completion] =
    for ((name, type_) ← expr.typeBindings)
      yield name -> getCompletion(name, type_)

  private def getBindingCompletions(bindings: Map[String, MashValue]): Map[String, Completion] =
    for ((name, value) ← bindings)
      yield name -> getCompletion(name, value)


  /**
    * Complete bindings at the given position (using a dummy identifier)
    */
  def completeBindings(programText: String,
                       bindings: Map[String, MashValue],
                       pos: Int,
                       parser: CompletionParser): Option[CompletionResult] = {
    val textWithDummyIdentifier = StringUtils.insert(programText, pos, " dummy ")
    val tokens = parser.tokenise(textWithDummyIdentifier)
    val dummyTokenStart = pos + 1
    for {
      identifierToken ← tokens.find(_.region contains dummyTokenStart)
      completionResult ← completeBindings(textWithDummyIdentifier, identifierToken, parser, bindings, filterToMatchIdentifierPrefix = false)
    } yield completionResult.copy(replacementLocation = Region(pos, 0))
  }

  private def getCompletion(name: String, value: MashValue): Completion = value match {
    case mf: MashFunction ⇒
      Completion(name, typeOpt = Some(CompletionType.Function), descriptionOpt = mf.summaryOpt orElse Some(name))
    case bf: BoundMethod  ⇒
      Completion(name, typeOpt = Some(CompletionType.Method), descriptionOpt = bf.method.summaryOpt orElse Some(name))
    case klass: MashClass  ⇒
      Completion(name, typeOpt = Some(CompletionType.Class), descriptionOpt = klass.summaryOpt orElse Some(name))
    case _                ⇒
      Completion(name, typeOpt = Some(CompletionType.Binding), descriptionOpt = Some(ToStringifier.safeStringify(value)))
  }

  private def getCompletion(name: String, type_ : Type): Completion = type_ match {
    case Type.BoundBuiltinMethod(_, method) ⇒
      Completion(name, typeOpt = Some(CompletionType.Method), descriptionOpt = method.summaryOpt orElse Some(name))
    case Type.BoundUserDefinedMethod(_, method) ⇒
      Completion(name, typeOpt = Some(CompletionType.Method), descriptionOpt = method.docCommentOpt.map(_.summary) orElse method.nameOpt)
    case Type.BuiltinFunction(fun)          ⇒
      Completion(name, typeOpt = Some(CompletionType.Function), descriptionOpt = fun.summaryOpt orElse Some(name))
    case udf: Type.UserDefinedFunction          ⇒
      Completion(name, typeOpt = Some(CompletionType.Function), descriptionOpt = udf.docCommentOpt.map(_.summary) orElse udf.nameOpt)
    case Type.UserClass(name, _, _) ⇒
      Completion(name, typeOpt = Some(CompletionType.Class), descriptionOpt = Some(name))
    case _                                  ⇒
      Completion(name, typeOpt = Some(CompletionType.Binding))
  }

}