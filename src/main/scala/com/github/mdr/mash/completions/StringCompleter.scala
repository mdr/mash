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
  private val argCompleter = new ArgCompleter(fileSystem, envInteractions)

  def completeAsString(text: String, token: Token, parser: CompletionParser): StringCompletionResult =
    completeAsString(text, token.region, parser)

  /**
   * Reinterpret nearby characters as a String literal and attempt completion on that instead.
   */
  def completeAsString(text: String, initialRegion: Region, parser: CompletionParser): StringCompletionResult = {
    def completeAsString(liberal: Boolean): StringCompletionResult = {
      val contiguousRegion = ContiguousRegionFinder.getContiguousRegion(text, initialRegion, mish = parser.mish,
        liberal = liberal)
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
    lazy val argCompletionOpt = argCompleter.completeArg(text, stringRegion, parser)
    lazy val equalityCompletionOpt = EqualityCompleter.completeEquality(text, stringRegion, parser)
    lazy val pathCompletionOpt = {
      val withoutQuotes = stringRegion.of(text).filterNot(_ == '"')
      pathCompleter.completePaths(withoutQuotes, stringRegion)
    }
    val isPathCompletion = argCompletionOpt.isEmpty && equalityCompletionOpt.isEmpty && pathCompletionOpt.isDefined
    val completionResultOpt = argCompletionOpt orElse equalityCompletionOpt orElse pathCompletionOpt
    StringCompletionResult(isPathCompletion, completionResultOpt)
  }

}