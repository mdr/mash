package com.github.mdr.mash.completions

import com.github.mdr.mash.compiler.Compiler
import com.github.mdr.mash.evaluator.BoundMethod
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType
import com.github.mdr.mash.os.EnvironmentInteractions
import com.github.mdr.mash.os.FileSystem
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.utils.Utils
import com.github.mdr.mash.compiler.BareStringify

class UberCompleter(fileSystem: FileSystem, envInteractions: EnvironmentInteractions) {

  private val pathCompleter = new PathCompleter(fileSystem, envInteractions)
  private val stringCompleter = new StringCompleter(fileSystem, envInteractions)

  def complete(s: String, pos: Int, env: Environment, mish: Boolean): Option[CompletionResult] = {
    val parser = new CompletionParser(env, mish)
    findNearbyToken(s, pos, parser).flatMap(completeToken(s, pos, parser)).map(_.sorted)
  }

  private def completeToken(text: String, pos: Int, parser: CompletionParser)(nearbyToken: Token) =
    nearbyToken.tokenType match {
      case TokenType.STRING_LITERAL ⇒
        stringCompleter.completeString(text, nearbyToken, parser).completionResultOpt orElse
          stringCompleter.completeAsString(text, nearbyToken, parser).completionResultOpt
      case TokenType.LONG_FLAG ⇒
        val flagResultOpt = FlagCompleter.completeLongFlag(text, nearbyToken, parser)
        val asStringResultOpt = stringCompleter.completeAsString(text, nearbyToken, parser).completionResultOpt
        CompletionResult.merge(flagResultOpt, asStringResultOpt)
      case TokenType.MINUS ⇒
        val flagResultOpt = FlagCompleter.completeAllFlags(text, nearbyToken, parser)
        val asStringResultOpt = stringCompleter.completeAsString(text, nearbyToken, parser).completionResultOpt
        CompletionResult.merge(flagResultOpt, asStringResultOpt)
      case TokenType.IDENTIFIER ⇒
        completeIdentifier(text, nearbyToken, parser)
      case TokenType.DOT | TokenType.DOT_NULL_SAFE ⇒
        completeDot(text, nearbyToken, pos, parser)
      case _ ⇒
        completeMisc(text, nearbyToken, pos, parser)
    }

  private def completeMisc(text: String, nearbyToken: Token, pos: Int, parser: CompletionParser): Option[CompletionResult] = {
    val asStringRegion = if (nearbyToken.isWhitespace) Region(pos, 0) else nearbyToken.region
    val StringCompletionResult(isPathCompletion, asStringResultOpt) =
      stringCompleter.completeAsString(text, asStringRegion, parser)
    val bindingResultOpt = completeBindingsAndFiles(parser.env, prefix = "", Region(pos, 0))
    if (isPathCompletion)
      CompletionResult.merge(asStringResultOpt, bindingResultOpt)
    else
      asStringResultOpt orElse bindingResultOpt
  }

  private def completeIdentifier(text: String, identiferToken: Token, parser: CompletionParser): Option[CompletionResult] = {
    val StringCompletionResult(isPathCompletion, asStringResultOpt) =
      stringCompleter.completeAsString(text, identiferToken, parser)
    val MemberCompletionResult(isMemberExpr, memberResultOpt) =
      MemberCompleter.completeIdentifier(text, identiferToken, parser)
    val bindingResultOpt =
      if (isMemberExpr)
        None // It would be misleading to try and complete other things in member position
      else
        completeBindingsAndFiles(parser.env, identiferToken.text, identiferToken.region)
    if (isPathCompletion)
      CompletionResult.merge(asStringResultOpt, bindingResultOpt)
    else
      asStringResultOpt orElse memberResultOpt orElse bindingResultOpt
  }

  private def completeDot(text: String, dotToken: Token, pos: Int, parser: CompletionParser): Option[CompletionResult] = {
    lazy val memberResultOpt = MemberCompleter.completeAfterDot(text, dotToken, pos, parser)
    lazy val asStringResultOpt = stringCompleter.completeAsString(text, dotToken, parser).completionResultOpt

    // Prefer string completions in situations like "ls .emacs" by checking the char before the dot:
    val posBefore = dotToken.offset - 1
    val isMemberDot = posBefore >= 0 && !text(posBefore).isWhitespace

    // Prefer string completions for bare strings:
    val isAfterBareString: Boolean =
      (for {
        previousToken ← parser.tokenise(text).find(_.region.posAfter == dotToken.offset)
        bareTokens = parser.getBareTokens(text)
      } yield bareTokens.contains(previousToken)).getOrElse(false)

    if (isMemberDot && !isAfterBareString)
      memberResultOpt orElse asStringResultOpt
    else
      asStringResultOpt orElse memberResultOpt
  }

  /**
   * Find a nearby token that we'll use as the start point for the completion search
   */
  private def findNearbyToken(s: String, pos: Int, parser: CompletionParser): Option[Token] = {
    val tokens = parser.tokenise(s)
    val beforeTokenOpt = tokens.find(_.region.posAfter == pos)
    val onTokenOpt = tokens.find(_.region contains pos)
    Utils.optionCombine[Token](beforeTokenOpt, onTokenOpt, {
      case (beforeToken, onToken) if isPrimaryCompletionToken(onToken) ⇒ onToken
      case (beforeToken, onToken) if isPrimaryCompletionToken(beforeToken) ⇒ beforeToken
      case (beforeToken, onToken) ⇒ onToken
    })
  }

  private def isPrimaryCompletionToken(token: Token) = {
    import TokenType._
    Set[TokenType](STRING_LITERAL, IDENTIFIER, LONG_FLAG, MINUS, DOT, DOT_NULL_SAFE) contains token.tokenType
  }

  private def completeBindingsAndFiles(env: Environment, prefix: String, region: Region): Option[CompletionResult] = {
    val bindingCompletions =
      for {
        (name, value) ← env.valuesMap.toSeq
        if name startsWith prefix
        (completionType, description) = getBindingTypeAndDescription(value)
      } yield Completion(name, typeOpt = Some(completionType), descriptionOpt = Some(description))
    val bindingResultOpt = CompletionResult.of(bindingCompletions, region)
    val pathResultOpt = pathCompleter.completePaths(prefix, region)
    CompletionResult.merge(bindingResultOpt, pathResultOpt)
  }

  private def getBindingTypeAndDescription(value: Any) = value match {
    case mf: MashFunction ⇒ (CompletionType.Function, mf.summary)
    case bf: BoundMethod  ⇒ (CompletionType.Method, bf.method.summary)
    case x                ⇒ (CompletionType.Binding, x + "")
  }

}

case class CompletionParser(env: Environment, mish: Boolean) {

  def parse(s: String): Option[Expr] =
    Compiler.compile(s, env, forgiving = true, inferTypes = true, mish = mish)

  def tokenise(s: String): Seq[Token] =
    MashLexer.tokenise(s, forgiving = true, includeCommentsAndWhitespace = true, mish = mish)

  def getBareTokens(s: String): Seq[Token] =
    Compiler.compile(s, env, forgiving = true, mish = mish, bareWords = false).map(expr ⇒
      BareStringify.getBareTokens(expr, env.globalVariables.keySet.toSet).toSeq).getOrElse(Seq())

}
