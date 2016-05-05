package com.github.mdr.mash.completions

import scala.PartialFunction.condOpt

import com.github.mdr.mash.compiler.Compiler
import com.github.mdr.mash.evaluator.BoundMethod
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.evaluator.TildeExpander
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.inference.AnnotatedExpr
import com.github.mdr.mash.inference.SimpleTypedArguments
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.TypedArgument
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os.EnvironmentInteractions
import com.github.mdr.mash.os.FileSystem
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.StringEscapes
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils

class UberCompleter(fileSystem: FileSystem, envInteractions: EnvironmentInteractions) {

  private val pathCompleter = new PathCompleter(fileSystem)

  def complete(s: String, pos: Int, env: Environment, mish: Boolean): Option[CompletionResult] =
    findNearbyToken(s, pos, mish).flatMap(completeToken(s, pos, env, mish)).map(_.sorted)

  private def completeToken(s: String, pos: Int, env: Environment, mish: Boolean)(nearbyToken: Token) =
    nearbyToken.tokenType match {
      case TokenType.STRING_LITERAL ⇒
        completeString(s, nearbyToken.region, env, mish).completionResultOpt orElse
          completeAsString(s, nearbyToken.region, env, mish).completionResultOpt
      case TokenType.LONG_FLAG ⇒
        val exprOpt = Compiler.compile(s, env, forgiving = true, inferTypes = true, mish = mish)
        for {
          expr ← exprOpt
          sourceInfo ← expr.sourceInfoOpt
          InvocationInfo(invocationExpr, _) ← InvocationFinder.findInvocationWithFlagArg(expr, nearbyToken)
          functionType ← invocationExpr.function.typeOpt
          flags ← FlagCompleter.getFlags(functionType)
          completions = FlagCompleter.completeLongFlag(flags, nearbyToken)
          if completions.nonEmpty
        } yield CompletionResult(completions, nearbyToken.region)
      case TokenType.MINUS ⇒
        val replaced = StringUtils.replace(s, nearbyToken.region, "--dummyFlag")
        val exprOpt = Compiler.compile(replaced, env, forgiving = true, inferTypes = true, mish = mish)
        for {
          expr ← exprOpt
          sourceInfo ← expr.sourceInfoOpt
          tokens = sourceInfo.expr.tokens
          flagToken ← tokens.find(t ⇒ t.region.overlaps(nearbyToken.region) && t.isFlag)
          InvocationInfo(invocationExpr, _) ← InvocationFinder.findInvocationWithFlagArg(expr, flagToken)
          functionType ← invocationExpr.function.typeOpt
          flags ← FlagCompleter.getFlags(functionType)
          completions = FlagCompleter.completeAllFlags(flags)
          if completions.nonEmpty
        } yield CompletionResult(completions, nearbyToken.region)
      case TokenType.IDENTIFIER ⇒
        val StringCompletionResult(isPathCompletion, asStringResultOpt) = completeAsString(s, nearbyToken.region, env, mish = mish)
        val (isMemberExpr, memberResultOpt) = MemberCompleter.completeMember(s, nearbyToken, env, mish = mish)
        val bindingResultOpt =
          if (isMemberExpr)
            None // It would be misleading to try and complete other things in member position
          else
            completeBindingsAndFiles(env, nearbyToken.text, nearbyToken.region)
        if (isPathCompletion)
          CompletionResult.merge(asStringResultOpt, bindingResultOpt)
        else
          asStringResultOpt orElse memberResultOpt orElse bindingResultOpt
      case TokenType.DOT | TokenType.DOT_NULL_SAFE ⇒
        val posBeforeDot = nearbyToken.offset - 1
        val isMemberDot = (posBeforeDot >= 0 && !s(posBeforeDot).isWhitespace)
        val replaced = StringUtils.replace(s, nearbyToken.region, ".dummy")
        val expr = Compiler.compile(replaced, env, forgiving = true, inferTypes = true, mish = mish)
        val memberCompletionResultOpt = MemberCompleter.complete(s, nearbyToken, env, pos, mish = mish)
        if (isMemberDot)
          memberCompletionResultOpt orElse completeAsString(s, nearbyToken.region, env, mish = mish).completionResultOpt
        else
          completeAsString(s, nearbyToken.region, env, mish = mish).completionResultOpt orElse memberCompletionResultOpt
      case t ⇒
        val asStringRegion = if (t.isWhitespace) Region(pos, 0) else nearbyToken.region
        val StringCompletionResult(isPathCompletion, asStringResultOpt) = completeAsString(s, asStringRegion, env, mish = mish)
        val bindingResultOpt = completeBindingsAndFiles(env, prefix = "", Region(pos, 0))
        if (isPathCompletion)
          CompletionResult.merge(asStringResultOpt, bindingResultOpt)
        else
          asStringResultOpt orElse bindingResultOpt
    }

  /**
   * Find a nearby token that we'll use as the start point for the completion search
   */
  private def findNearbyToken(s: String, pos: Int, mish: Boolean): Option[Token] = {
    val tokens = MashLexer.tokenise(s, forgiving = true, includeCommentsAndWhitespace = true, mish = mish)
    tokens.find(t ⇒ t.region.contains(pos) && isPrimaryCompletionToken(t)) orElse
      tokens.find(t ⇒ pos == t.region.posAfter || t.region.contains(pos))
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
    val fileCompletions = completePaths(prefix)
    val completions = bindingCompletions ++ fileCompletions
    if (completions.isEmpty)
      None
    else
      Some(CompletionResult(completions, region))
  }

  private def getBindingTypeAndDescription(value: Any) = value match {
    case mf: MashFunction ⇒ (CompletionType.Function, mf.summary)
    case bf: BoundMethod  ⇒ (CompletionType.Method, bf.method.summary)
    case x                ⇒ (CompletionType.Binding, x + "")
  }

  case class StringCompletionResult(isPathCompletion: Boolean, completionResultOpt: Option[CompletionResult]) {

    def orElse(that: ⇒ StringCompletionResult): StringCompletionResult =
      if (completionResultOpt.isDefined) this else that

    def withReplacementLocation(region: Region): StringCompletionResult =
      copy(completionResultOpt = completionResultOpt.map(res ⇒ res.copy(replacementLocation = region)))

  }

  /**
   * Reinterpret nearby characters as a String literal and attempt completion
   */
  private def completeAsString(s: String, initialRegion: Region, env: Environment, mish: Boolean): StringCompletionResult = {
    def completeAsString(liberal: Boolean): StringCompletionResult = {
      val contiguousRegion = ContiguousRegionFinder.getContiguousRegion(s, initialRegion, mish = mish, liberal = liberal)
      val replacement = '"' + contiguousRegion.of(s).filterNot('"' == _) + '"'
      val replaced = StringUtils.replace(s, contiguousRegion, replacement)
      val stringRegion = contiguousRegion.copy(length = replacement.length)
      completeString(replaced, stringRegion, env, mish).withReplacementLocation(contiguousRegion)
    }
    completeAsString(liberal = true) orElse completeAsString(liberal = false)
  }

  /**
   * Provide completions for the string literal at the given position
   */
  private def completeString(s: String, stringRegion: Region, env: Environment, mish: Boolean): StringCompletionResult = {
    val exprAndTokenOpt =
      for {
        expr ← Compiler.compile(s, env, forgiving = true, inferTypes = true, mish = mish)
        sourceInfo ← expr.sourceInfoOpt
        tokens = sourceInfo.expr.tokens
        literalToken ← tokens.find(_.region == stringRegion)
      } yield (expr, literalToken)
    lazy val argCompletionOpt =
      for {
        (expr, literalToken) ← exprAndTokenOpt
        InvocationInfo(invocationExpr, argPos) ← InvocationFinder.findInvocationWithLiteralArg(expr, literalToken)
        completionSpecs ← getCompletionSpecs(invocationExpr, argPos)
        completions = completeFromSpecs(completionSpecs, literalToken)
        if completions.nonEmpty
      } yield CompletionResult(completions, stringRegion)
    lazy val equalityCompletionOpt =
      for {
        (expr, literalToken) ← exprAndTokenOpt
        equalityType ← EqualityFinder.findEqualityExprWithLiteralArg(expr, literalToken)
        completions ← getEqualityCompletions(equalityType, literalToken)
        if completions.nonEmpty
      } yield CompletionResult(completions, stringRegion)
    lazy val pathCompletionOpt = {
      val withoutQuotes = stringRegion.of(s).filterNot(_ == '"')
      val completions = completePaths(withoutQuotes)
      if (completions.nonEmpty)
        Some(CompletionResult(completions, stringRegion))
      else
        None
    }
    val isPathCompletion = argCompletionOpt.isEmpty && equalityCompletionOpt.isEmpty && pathCompletionOpt.isDefined
    val completionResultOpt = argCompletionOpt orElse equalityCompletionOpt orElse pathCompletionOpt
    StringCompletionResult(isPathCompletion, completionResultOpt)
  }

  private def getEqualityCompletions(t: Type, literalToken: Token): Option[Seq[Completion]] = condOpt(t) {
    case Type.Tagged(baseClass, tagClass) ⇒
      val withoutQuotes = literalToken.text.filterNot(_ == '"')
      tagClass.enumerationValues.toSeq.flatten.filter(_.startsWith(withoutQuotes)).map(Completion(_, isQuoted = baseClass == StringClass))
  }

  private def getCompletionSpecs(invocationExpr: InvocationExpr, argPos: Int): Option[Seq[CompletionSpec]] =
    invocationExpr.function.typeOpt.collect {
      case Type.DefinedFunction(f) ⇒
        f.getCompletionSpecs(argPos, typedArguments(invocationExpr))
      case Type.BoundMethod(targetType, m) ⇒
        m.getCompletionSpecs(argPos, Some(targetType), typedArguments(invocationExpr))
    }

  private def typedArguments(invocationExpr: InvocationExpr): TypedArguments = {
    def annotateArg(arg: Argument): TypedArgument =
      arg match {
        case Argument.PositionArg(e, _)           ⇒ TypedArgument.PositionArg(AnnotatedExpr(Some(e), e.typeOpt))
        case Argument.ShortFlag(flags, _)         ⇒ TypedArgument.ShortFlag(flags)
        case Argument.LongFlag(flag, valueOpt, _) ⇒ TypedArgument.LongFlag(flag, valueOpt.map(e ⇒ AnnotatedExpr(Some(e), e.typeOpt)))
      }
    SimpleTypedArguments(invocationExpr.arguments.map(annotateArg))
  }

  private def completePaths(s: String, directoriesOnly: Boolean = false): Seq[Completion] = {
    val tildeExpander = new TildeExpander(envInteractions)
    val tildeExpandedOpt = tildeExpander.expandOpt(s)
    val prefix = StringEscapes.unescape(tildeExpandedOpt.getOrElse(s))
    for {
      PathCompletion(path, typeOpt) ← pathCompleter.getCompletions(prefix, directoriesOnly = directoriesOnly)
      tildeExpanded = if (tildeExpandedOpt.isDefined) tildeExpander.retilde(path) else path
      escaped = StringEscapes.escapeChars(tildeExpanded)
    } yield Completion(tildeExpanded, Some(escaped), isQuoted = true, typeOpt = typeOpt, descriptionOpt = Some(path))
  }

  private def completeFromSpecs(completionSpecs: Seq[CompletionSpec], literalToken: Token): Seq[Completion] =
    completionSpecs.flatMap(spec ⇒ completeFromSpec(spec, literalToken))

  private def completeFromSpec(spec: CompletionSpec, literalToken: Token): Seq[Completion] = {
    import CompletionSpec._
    val withoutQuotes = literalToken.text.filterNot(_ == '"')
    spec match {
      case Directory | File ⇒
        completePaths(withoutQuotes, directoriesOnly = spec == Directory)
      case Members(targetType) ⇒
        MemberCompleter.complete(targetType, withoutQuotes)
    }
  }

}