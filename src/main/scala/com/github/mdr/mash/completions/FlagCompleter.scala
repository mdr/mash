package com.github.mdr.mash.completions

import scala.PartialFunction.condOpt

import com.github.mdr.mash.functions.Flag
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType

object FlagCompleter {

  private val LongFlagPrefix = "--"
  private val ShortFlagPrefix = "-"

  def getFlags(functionType: Type): Option[Seq[Flag]] = condOpt(functionType) {
    case Type.DefinedFunction(f) ⇒ f.flags
    case Type.BoundMethod(_, m)  ⇒ m.flags
  }

  def completeLongFlag(flags: Seq[Flag], flagToken: Token): Seq[Completion] = {
    val prefix = flagToken.text.drop(LongFlagPrefix.length)
    flags.collect {
      case Flag(description, _, Some(longName)) if longName startsWith prefix ⇒
        Completion(LongFlagPrefix + longName, descriptionOpt = Some(description), typeOpt = Some(CompletionType.Flag))
    }
  }

  def completeAllFlags(flags: Seq[Flag]): Seq[Completion] = flags.flatMap {
    case Flag(description, shortNameOpt, longNameOpt) ⇒
      (longNameOpt.map(LongFlagPrefix + _).toSeq ++ shortNameOpt.map(ShortFlagPrefix + _).toSeq)
        .map(Completion(_, descriptionOpt = Some(description)))
  }

}
