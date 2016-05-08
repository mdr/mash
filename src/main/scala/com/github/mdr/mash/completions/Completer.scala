package com.github.mdr.mash.completions

import com.github.mdr.mash.evaluator.BoundMethod
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType
import com.github.mdr.mash.os.EnvironmentInteractions
import com.github.mdr.mash.os.FileSystem
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.Utils

object Completer {

  private val PrimaryTokens: Set[TokenType] = {
    import TokenType._
    Set[TokenType](STRING_LITERAL, IDENTIFIER, LONG_FLAG, MINUS, DOT, DOT_NULL_SAFE)
  }

}

class Completer(fileSystem: FileSystem, envInteractions: EnvironmentInteractions) {

  import Completer._

  private val pathCompleter = new PathCompleter(fileSystem, envInteractions)
  private val stringCompleter = new StringCompleter(fileSystem, envInteractions)

  def complete(s: String, pos: Int, env: Environment, mish: Boolean): Option[CompletionResult] = {
    val parser = new CompletionParser(env, mish)
    findNearbyToken(s, pos, parser).flatMap(completeToken(s, pos, parser)).map(_.sorted)
  }

  /**
   * Find a nearby token to use as the start point for the completion search
   */
  private def findNearbyToken(s: String, pos: Int, parser: CompletionParser): Option[Token] = {
    val tokens = parser.tokenise(s)
    val beforeTokenOpt = tokens.find(_.region.posAfter == pos)
    val onTokenOpt = tokens.find(_.region contains pos)
    def isPrimary(token: Token) = PrimaryTokens contains token.tokenType
    Utils.optionCombine[Token](beforeTokenOpt, onTokenOpt, {
      case (beforeToken, onToken) if isPrimary(onToken)     ⇒ onToken
      case (beforeToken, onToken) if isPrimary(beforeToken) ⇒ beforeToken
      case (beforeToken, onToken)                           ⇒ onToken
    })
  }

  private def completeToken(text: String, pos: Int, parser: CompletionParser)(nearbyToken: Token) =
    nearbyToken.tokenType match {
      case TokenType.STRING_LITERAL ⇒
        completeStringLiteral(text, nearbyToken, parser)
      case TokenType.LONG_FLAG ⇒
        completeLongFlag(text, nearbyToken, parser)
      case TokenType.MINUS ⇒
        completeMinus(text, nearbyToken, parser)
      case TokenType.IDENTIFIER ⇒
        completeIdentifier(text, nearbyToken, parser)
      case TokenType.DOT | TokenType.DOT_NULL_SAFE ⇒
        completeDot(text, nearbyToken, pos, parser)
      case _ ⇒
        completeMisc(text, nearbyToken, pos, parser)
    }

  private def completeStringLiteral(text: String, stringLiteral: Token, parser: CompletionParser): Option[CompletionResult] =
    stringCompleter.completeString(text, stringLiteral, parser).completionResultOpt orElse
      stringCompleter.completeAsString(text, stringLiteral, parser).completionResultOpt

  private def completeLongFlag(text: String, flag: Token, parser: CompletionParser): Option[CompletionResult] = {
    val flagResultOpt = FlagCompleter.completeLongFlag(text, flag, parser)
    val asStringResultOpt = stringCompleter.completeAsString(text, flag, parser).completionResultOpt
    CompletionResult.merge(flagResultOpt, asStringResultOpt)
  }

  private def completeMinus(text: String, minus: Token, parser: CompletionParser): Option[CompletionResult] = {
    val flagResultOpt = FlagCompleter.completeAllFlags(text, minus, parser)
    val asStringResultOpt = stringCompleter.completeAsString(text, minus, parser).completionResultOpt
    CompletionResult.merge(flagResultOpt, asStringResultOpt)
  }

  private def completeIdentifier(text: String, identiferToken: Token, parser: CompletionParser): Option[CompletionResult] = {
    val StringCompletionResult(isPathCompletion, asStringResultOpt) =
      stringCompleter.completeAsString(text, identiferToken, parser)
    val MemberCompletionResult(isMemberExpr, memberResultOpt) =
      MemberCompleter.completeIdentifier(text, identiferToken, parser)
    val bindingResultOpt =
      if (isMemberExpr)
        None // It would be misleading to try and complete bindings in member position
      else
        BindingCompleter.completeBindings(parser.env, identiferToken.text, identiferToken.region)
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

  private def completeMisc(text: String, nearbyToken: Token, pos: Int, parser: CompletionParser): Option[CompletionResult] = {
    val asStringRegion = if (nearbyToken.isWhitespace) Region(pos, 0) else nearbyToken.region
    val StringCompletionResult(isPathCompletion, asStringResultOpt) =
      stringCompleter.completeAsString(text, asStringRegion, parser)
    val bindingResultOpt = BindingCompleter.completeBindings(parser.env, prefix = "", Region(pos, 0))
    // Path completions should merge with binding completions, other types of string completions should take priority
    // over binding completions:
    if (isPathCompletion)
      CompletionResult.merge(asStringResultOpt, bindingResultOpt)
    else
      asStringResultOpt orElse bindingResultOpt
  }

}