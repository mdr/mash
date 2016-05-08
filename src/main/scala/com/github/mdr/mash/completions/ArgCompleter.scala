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

class ArgCompleter(fileSystem: FileSystem, envInteractions: EnvironmentInteractions) {

  private val pathCompleter = new PathCompleter(fileSystem, envInteractions)

  def completeArg(text: String, stringRegion: Region, parser: CompletionParser): Option[CompletionResult] =
    for {
      expr ← parser.parse(text)
      sourceInfo ← expr.sourceInfoOpt
      tokens = sourceInfo.expr.tokens
      literalToken ← tokens.find(_.region == stringRegion)
      InvocationInfo(invocationExpr, argPos) ← InvocationFinder.findInvocationWithLiteralArg(expr, literalToken)
      completionSpecs ← getCompletionSpecs(invocationExpr, argPos)
      result ← completeFromSpecs(completionSpecs, literalToken)
    } yield result

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