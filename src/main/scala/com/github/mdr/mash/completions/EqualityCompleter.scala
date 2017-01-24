package com.github.mdr.mash.completions

import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.utils.Region

import scala.PartialFunction.condOpt

object EqualityCompleter {

  def completeEquality(text: String, stringRegion: Region, parser: CompletionParser): Option[CompletionResult] = {
    val expr = parser.parse(text)
    for {
      sourceInfo ← expr.sourceInfoOpt
      tokens = sourceInfo.node.tokens
      literalToken ← tokens.find(_.region == stringRegion)
      equalityType ← EqualityFinder.findEqualityExprWithLiteralArg(expr, literalToken)
      completions ← getEqualityCompletions(equalityType, literalToken)
      result ← CompletionResult.of(completions, stringRegion)
    } yield result
  }

  private def getEqualityCompletions(equalityType: Type, literalToken: Token): Option[Seq[Completion]] =
    condOpt(equalityType) {
      case Type.Tagged(baseClass, tagClass) ⇒
        val withoutQuotes = literalToken.text.filterNot(_ == '"')
        tagClass.enumerationValues.toSeq.flatten
          .filter(_.startsWith(withoutQuotes))
          .map(Completion(_, isQuoted = baseClass == StringClass))
    }

}