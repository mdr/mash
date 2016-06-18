package com.github.mdr.mash.editor

import com.github.mdr.mash.repl.LineBuffer
import com.github.mdr.mash.completions.ContiguousRegionFinder
import com.github.mdr.mash.parser.StringEscapes
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.lexer.Token

object QuoteToggler {

  private val DoubleQuote = "\""

  def toggleQuotes(lineBuffer: LineBuffer, mish: Boolean): LineBuffer = {
    val text = lineBuffer.text
    val cursorPos = lineBuffer.cursorPos

    val cursorToken = findCursorToken(text, mish, cursorPos).getOrElse(return lineBuffer)
    val cursorRegion = cursorToken.region
    val targetRegion = ContiguousRegionFinder.getContiguousRegion(text, cursorRegion, mish = mish, liberal = true)
    val targetText = targetRegion.of(text)
    val isAlreadyQuoted = isQuoted(targetText, DoubleQuote) || isQuoted(targetText, "'")
    if (isAlreadyQuoted)
      unquote(targetText, cursorPos, text, targetRegion)
    else
      quote(targetText, cursorPos, text, targetRegion)
  }

  private def isQuoted(s: String, delimiter: String): Boolean =
    s.startsWith(delimiter) && s.endsWith(delimiter) && s.size > 1

  /**
   * Find a non-whitespace token that is either under the cursor or immediately to the left of it.
   */
  private def findCursorToken(text: String, mish: Boolean, cursorPos: Int): Option[Token] = {
    val tokens = MashLexer.tokenise(text, forgiving = true, includeCommentsAndWhitespace = true, mish = mish)
    tokens.find(_.region contains cursorPos).filterNot(t ⇒ t.isWhitespace || t.isEof)
      .orElse(tokens.find(_.region.posAfter == cursorPos).filterNot(t ⇒ t.isWhitespace || t.isEof))
  }

  private def unquote(targetText: String, cursorPos: Int, text: String, region: Region): LineBuffer = {
    val inner = targetText.tail.init
    val unescaped = StringEscapes.unescape(inner)
    val unescapesOccurred = unescaped.length < inner.length
    val newText = region.replace(text, unescaped)
    val newCursorPos = calculateCursorPosAfterUnquoting(unescapesOccurred, region, unescaped, cursorPos)
    LineBuffer(newText, newCursorPos)
  }

  private def calculateCursorPosAfterUnquoting(unescapesOccurred: Boolean, region: Region, unescaped: String, cursorPos: Int) =
    if (unescapesOccurred)
      region.offset + unescaped.length
    else if (cursorPos == region.offset)
      cursorPos
    else if (region contains cursorPos)
      cursorPos - 1
    else
      cursorPos - 2

  private def quote(targetText: String, cursorPos: Int, text: String, targetRegion: Region): LineBuffer = {
    val stripResult = stripQuotes(targetText)
    val inner = stripResult.stripped
    val escaped = StringEscapes.escapeChars(inner)
    val escapesOccurred = escaped.length > inner.length
    val quoted = DoubleQuote + escaped + DoubleQuote
    val newText = targetRegion.replace(text, quoted)
    val newCursorPos =
      calculateCursorPosAfterQuoting(escapesOccurred, cursorPos, targetRegion, stripResult, quoted)
    LineBuffer(newText, newCursorPos)
  }

  private case class StripResult(stripped: String, initialQuoteRemoved: Boolean, finalQuoteRemoved: Boolean)

  private def stripQuotes(targetText: String): StripResult = {
    var inner = targetText
    var initialQuoteRemoved = false
    if (targetText startsWith DoubleQuote) {
      inner = inner.tail
      initialQuoteRemoved = true
    }
    var finalQuoteRemoved = false
    if (inner endsWith DoubleQuote) {
      inner = inner.init
      finalQuoteRemoved = true
    }
    StripResult(inner, initialQuoteRemoved, finalQuoteRemoved)
  }

  private def calculateCursorPosAfterQuoting(escapesOccurred: Boolean, cursorPos: Int, replacementRegion: Region, stripResult: StripResult, quoted: String) =
    if (escapesOccurred) // Keeping track of the cursor if there has been escapes is more complex; for now we move the cursor to the end of the string
      replacementRegion.offset + quoted.length
    else {
      val shift =
        if (cursorPos <= replacementRegion.lastPos) {
          if (stripResult.initialQuoteRemoved) 0 else 1
        } else {
          if (stripResult.initialQuoteRemoved || stripResult.finalQuoteRemoved) 1 else 2
        }
      cursorPos + shift
    }
}