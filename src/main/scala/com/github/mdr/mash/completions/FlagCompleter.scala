package com.github.mdr.mash.completions

import com.github.mdr.mash.functions.Flag
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.utils.StringUtils

import scala.PartialFunction.condOpt

object FlagCompleter {

  private val LongFlagPrefix = "--"
  private val ShortFlagPrefix = "-"

  /**
   * Given a long flag token, find completions for which it is a prefix.
   */
  def completeLongFlag(text: String, flagToken: Token, parser: CompletionParser): Option[CompletionResult] = {
    val expr = parser.parse(text)
    for {
      sourceInfo ← expr.sourceInfoOpt
      InvocationInfo(invocationExpr, _) ← InvocationFinder.findInvocationWithFlagArg(expr, flagToken)
      functionType ← invocationExpr.function.typeOpt
      flags ← getFlags(functionType)
      completions = completeLongFlag(flags, flagToken)
      result ← CompletionResult.of(completions, flagToken.region)
    } yield result
  }
  private def completeLongFlag(flags: Seq[Flag], flagToken: Token): Seq[Completion] = {
    val prefix = flagToken.text.drop(LongFlagPrefix.length)
    flags.collect {
      case Flag(description, _, Some(longName)) if longName startsWith prefix ⇒
        Completion(LongFlagPrefix + longName, descriptionOpt = Some(description), typeOpt = Some(CompletionType.Flag))
    }
  }

  /**
   * Given a hyphen, provide completion options for all possible flags
   */
  def completeAllFlags(text: String, minusToken: Token, parser: CompletionParser): Option[CompletionResult] = {
    val textWithDummyFlag = StringUtils.replace(text, minusToken.region, "--dummyFlag ")
    val expr = parser.parse(textWithDummyFlag)
    for {
      sourceInfo ← expr.sourceInfoOpt
      tokens = sourceInfo.expr.tokens
      flagToken ← tokens.find(t ⇒ t.isFlag && t.region.overlaps(minusToken.region))
      InvocationInfo(invocationExpr, _) ← InvocationFinder.findInvocationWithFlagArg(expr, flagToken)
      functionType ← invocationExpr.function.typeOpt
      flags ← getFlags(functionType)
      completions = flags.flatMap(getCompletions)
      result ← CompletionResult.of(completions, minusToken.region)
    } yield result
  }

  /**
   * Return the possible completions from this flag (there can be up to two -- one from the short form, one from
   * the long).
   */
  private def getCompletions(flag: Flag): Seq[Completion] = {
    val Flag(description, shortNameOpt, longNameOpt) = flag
    val longFlags = longNameOpt.map(LongFlagPrefix + _).toSeq
    val shortFlags = shortNameOpt.map(ShortFlagPrefix + _).toSeq
    val flags = longFlags ++ shortFlags
    flags.map(Completion(_, descriptionOpt = Some(description)))
  }

  private def getFlags(functionType: Type): Option[Seq[Flag]] = condOpt(functionType) {
    case Type.BuiltinFunction(f)       ⇒ f.flags
    case Type.BoundBuiltinMethod(_, m) ⇒ m.flags
  }

}
