package com.github.mdr.mash.completions

import scala.PartialFunction.condOpt

import com.github.mdr.mash.inference._
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os.EnvironmentInteractions
import com.github.mdr.mash.os.FileSystem
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils

case class StringCompletionResult(isPathCompletion: Boolean, completionResultOpt: Option[CompletionResult]) {

  def orElse(that: ⇒ StringCompletionResult): StringCompletionResult =
    if (completionResultOpt.isDefined) this else that

  def withReplacementLocation(region: Region): StringCompletionResult =
    copy(completionResultOpt = completionResultOpt.map(res ⇒ res.copy(replacementLocation = region)))

}

class StringCompleter(fileSystem: FileSystem, envInteractions: EnvironmentInteractions) {

  private val pathCompleter = new PathCompleter(fileSystem, envInteractions)

  def completeAsString(text: String, token: Token, parser: CompletionParser): StringCompletionResult =
    completeAsString(text, token.region, parser)

  /**
   * Reinterpret nearby characters as a String literal and attempt completion on that instead.
   */
  def completeAsString(text: String, initialRegion: Region, parser: CompletionParser): StringCompletionResult = {
    def completeAsString(liberal: Boolean): StringCompletionResult = {
      val contiguousRegion = ContiguousRegionFinder.getContiguousRegion(text, initialRegion, mish = parser.mish, liberal = liberal)
      val replacement = '"' + contiguousRegion.of(text).filterNot('"' == _) + '"'
      val replaced = StringUtils.replace(text, contiguousRegion, replacement)
      val stringRegion = contiguousRegion.copy(length = replacement.length)
      completeString(replaced, stringRegion, parser).withReplacementLocation(contiguousRegion)
    }
    completeAsString(liberal = true) orElse completeAsString(liberal = false)
  }

  def completeString(text: String, token: Token, parser: CompletionParser): StringCompletionResult =
    completeString(text, token.region, parser)

  /**
   * Provide completions for the string literal at the given position, attempting several strategies:
   * - if it's an argument to a function/method, see if there are any special strategies provided for completing
   *    that argument (for example, the argument should be a directory)
   * - if it's one side of an equality or inequality expression, provide completions based on the type of the other side
   * - complete paths
   */
  def completeString(text: String, stringRegion: Region, parser: CompletionParser): StringCompletionResult = {
    val exprAndTokenOpt =
      for {
        expr ← parser.parse(text)
        sourceInfo ← expr.sourceInfoOpt
        tokens = sourceInfo.expr.tokens
        literalToken ← tokens.find(_.region == stringRegion)
      } yield (expr, literalToken)
    lazy val argCompletionOpt =
      for {
        (expr, literalToken) ← exprAndTokenOpt
        InvocationInfo(invocationExpr, argPos) ← InvocationFinder.findInvocationWithLiteralArg(expr, literalToken)
        completionSpecs ← getCompletionSpecs(invocationExpr, argPos)
        result ← completeFromSpecs(completionSpecs, literalToken)
      } yield result
    lazy val equalityCompletionOpt =
      for {
        (expr, literalToken) ← exprAndTokenOpt
        equalityType ← EqualityFinder.findEqualityExprWithLiteralArg(expr, literalToken)
        completions ← getEqualityCompletions(equalityType, literalToken)
        result ← CompletionResult.of(completions, stringRegion)
      } yield result
    lazy val pathCompletionOpt = {
      val withoutQuotes = stringRegion.of(text).filterNot(_ == '"')
      pathCompleter.completePaths(withoutQuotes, stringRegion)
    }
    val isPathCompletion = argCompletionOpt.isEmpty && equalityCompletionOpt.isEmpty && pathCompletionOpt.isDefined
    val completionResultOpt = argCompletionOpt orElse equalityCompletionOpt orElse pathCompletionOpt
    StringCompletionResult(isPathCompletion, completionResultOpt)
  }

  private def getEqualityCompletions(t: Type, literalToken: Token): Option[Seq[Completion]] = condOpt(t) {
    case Type.Tagged(baseClass, tagClass) ⇒
      val withoutQuotes = literalToken.text.filterNot(_ == '"')
      tagClass.enumerationValues.toSeq.flatten
        .filter(_.startsWith(withoutQuotes))
        .map(Completion(_, isQuoted = baseClass == StringClass))
  }

  private def getCompletionSpecs(invocationExpr: InvocationExpr, argPos: Int): Option[Seq[CompletionSpec]] =
    invocationExpr.function.typeOpt.collect {
      case Type.DefinedFunction(f) ⇒
        f.getCompletionSpecs(argPos, typedArguments(invocationExpr))
      case Type.BoundMethod(targetType, m) ⇒
        m.getCompletionSpecs(argPos, Some(targetType), typedArguments(invocationExpr))
    }

  private def annotateArg(arg: Argument): TypedArgument = arg match {
    case Argument.PositionArg(e, _)           ⇒ TypedArgument.PositionArg(AnnotatedExpr(Some(e), e.typeOpt))
    case Argument.ShortFlag(flags, _)         ⇒ TypedArgument.ShortFlag(flags)
    case Argument.LongFlag(flag, valueOpt, _) ⇒ TypedArgument.LongFlag(flag, valueOpt.map(e ⇒ AnnotatedExpr(Some(e), e.typeOpt)))
  }

  private def typedArguments(invocationExpr: InvocationExpr): TypedArguments =
    SimpleTypedArguments(invocationExpr.arguments.map(annotateArg))

  private def completeFromSpecs(completionSpecs: Seq[CompletionSpec], literalToken: Token): Option[CompletionResult] =
    completionSpecs.map(spec ⇒ completeFromSpec(spec, literalToken)).fold(None)(CompletionResult.merge)

  private def completeFromSpec(spec: CompletionSpec, literalToken: Token): Option[CompletionResult] = {
    val withoutQuotes = literalToken.text.filterNot(_ == '"')
    import CompletionSpec._
    spec match {
      case Directory | File ⇒
        pathCompleter.completePaths(withoutQuotes, literalToken.region, directoriesOnly = spec == Directory)
      case Members(targetType) ⇒
        val members = MemberCompleter.completeString(targetType, withoutQuotes)
        CompletionResult.of(members, literalToken.region)
    }
  }

}