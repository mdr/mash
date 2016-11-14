package com.github.mdr.mash.completions


import com.github.mdr.mash.inference._
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.os.{ EnvironmentInteractions, FileSystem }
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.utils.Region

class ArgCompleter(fileSystem: FileSystem, envInteractions: EnvironmentInteractions) {

  private val pathCompleter = new PathCompleter(fileSystem, envInteractions)

  def completeArg(text: String, stringRegion: Region, parser: CompletionParser): Option[CompletionResult] = {
    val expr = parser.parse(text)
    for {
      sourceInfo ← parser.parse(text).sourceInfoOpt
      tokens = sourceInfo.expr.tokens
      literalToken ← tokens.find(_.region == stringRegion)
      InvocationInfo(invocationExpr, argPos) ← InvocationFinder.findInvocationWithLiteralArg(expr, literalToken)
      completionSpecs ← getCompletionSpecs(invocationExpr, argPos)
      result ← completeFromSpecs(completionSpecs, literalToken)
    } yield result
  }

  private def getCompletionSpecs(invocationExpr: InvocationExpr, argPos: Int): Option[Seq[CompletionSpec]] =
    invocationExpr.function.typeOpt.collect {
      case Type.DefinedFunction(f) ⇒
        f.getCompletionSpecs(argPos, SimpleTypedArguments.from(invocationExpr))
      case Type.BoundMethod(targetType, m) ⇒
        m.getCompletionSpecs(argPos, Some(targetType), SimpleTypedArguments.from(invocationExpr))
    }

  private def completeFromSpecs(completionSpecs: Seq[CompletionSpec], literalToken: Token): Option[CompletionResult] =
    completionSpecs.map(spec ⇒ completeFromSpec(spec, literalToken)).fold(None)(CompletionResult.merge)

  private def completeFromSpec(spec: CompletionSpec, literalToken: Token): Option[CompletionResult] = {
    val withoutQuotes = literalToken.text.filterNot(_ == '"')
    import CompletionSpec._
    spec match {
      case Directory | File ⇒
        def completePaths(substring: Boolean) =
          pathCompleter.completePaths(withoutQuotes, literalToken.region, directoriesOnly = spec == Directory, substring = substring)
        completePaths(substring = false) orElse completePaths(substring = true)
      case Members(targetType) ⇒
        val members = MemberCompleter.completeString(targetType, withoutQuotes)
        CompletionResult.of(members, literalToken.region)
      case Items(items) ⇒
        val matches = items.filter(_.startsWith(withoutQuotes))
        val prefixResult = CompletionResult.of(matches.map(s ⇒ Completion(s, isQuoted = true)), literalToken.region)
        prefixResult.orElse { // substring:
          val matches = items.filter(_.contains(withoutQuotes))
          def makeCompletion(item: String): Completion = {
            val index = item.indexOf(withoutQuotes)
            val location = CompletionLocation(index, index)
            Completion(item, isQuoted = true, location = location)
          }
          CompletionResult.of(matches.map(makeCompletion), literalToken.region)
        }

    }
  }

}