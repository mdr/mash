package com.github.mdr.mash.completions

import scala.PartialFunction.condOpt

import com.github.mdr.mash.functions.Flag
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.Token

object FlagCompleter {

  private val LongFlagPrefix = "--"
  private val ShortFlagPrefix = "-"

  def completeLongFlag(functionType: Type, flagToken: Token): Option[CompletionResult] =
    for {
      flags ← getFlags(functionType)
      completions = completeLongFlag(flags, flagToken)
      if completions.nonEmpty
    } yield CompletionResult(completions, flagToken.region)

  private def completeLongFlag(flags: Seq[Flag], flagToken: Token): Seq[Completion] = {
    val prefix = flagToken.text.drop(LongFlagPrefix.length)
    flags.collect {
      case Flag(description, _, Some(longName)) if longName startsWith prefix ⇒
        Completion(LongFlagPrefix + longName, descriptionOpt = Some(description), typeOpt = Some(CompletionType.Flag))
    }
  }

  def completeAllFlags(functionType: Type, flagToken: Token): Option[CompletionResult] =
    for {
      flags ← getFlags(functionType)
      completions = completeAllFlags(flags)
      if completions.nonEmpty
    } yield CompletionResult(completions, flagToken.region)

  private def completeAllFlags(flags: Seq[Flag]): Seq[Completion] = flags.flatMap {
    case Flag(description, shortNameOpt, longNameOpt) ⇒
      val longFlags = longNameOpt.map(LongFlagPrefix + _).toSeq
      val shortFlags = shortNameOpt.map(ShortFlagPrefix + _).toSeq
      val flags = longFlags ++ shortFlags
      flags.map(Completion(_, descriptionOpt = Some(description)))
  }

  private def getFlags(functionType: Type): Option[Seq[Flag]] = condOpt(functionType) {
    case Type.DefinedFunction(f) ⇒ f.flags
    case Type.BoundMethod(_, m)  ⇒ m.flags
  }

}
