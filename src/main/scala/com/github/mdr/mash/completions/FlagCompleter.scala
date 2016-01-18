package com.github.mdr.mash.completions

import scala.PartialFunction.condOpt

import com.github.mdr.mash.functions.Flag
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType

object FlagCompleter {

  def getFlags(functionType: Type): Option[Seq[Flag]] = condOpt(functionType) {
    case Type.DefinedFunction(f) ⇒ f.flags
    case Type.BoundMethod(_, m)  ⇒ m.flags
  }

  def completeFlag(flags: Seq[Flag], flagToken: Token): Seq[Completion] = (flagToken.tokenType match {
    case TokenType.LONG_FLAG ⇒
      val soFar = flagToken.text.drop(2)
      flags.collect {
        case Flag(description, _, Some(longFlag)) if longFlag.startsWith(soFar) ⇒
          Completion("--" + longFlag, descriptionOpt = Some(description), completionTypeOpt = Some(CompletionType.Flag))
      }
    case TokenType.SHORT_FLAG ⇒
      val soFar = flagToken.text.drop(1)
      flags.collect {
        case Flag(description, Some(shortFlag), _) if shortFlag.startsWith(soFar) ⇒
          Completion("--" + shortFlag, descriptionOpt = Some(description), completionTypeOpt = Some(CompletionType.Flag))
      }
    case _ ⇒ Seq()
  }).sortBy(_.text)

  def completeAllFlags(flags: Seq[Flag]): Seq[Completion] = flags.flatMap {
    case Flag(description, shortFlagOpt, longFlagOpt) ⇒
      (longFlagOpt.map("--" + _).toSeq ++ shortFlagOpt.map("-" + _).toSeq).map(Completion(_, descriptionOpt = Some(description)))
  }.sortBy(_.text)
}
