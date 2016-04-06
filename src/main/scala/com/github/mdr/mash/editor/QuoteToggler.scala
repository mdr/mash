package com.github.mdr.mash.editor

import com.github.mdr.mash.LineBuffer
import com.github.mdr.mash.completions.ContiguousRegionFinder
import com.github.mdr.mash.parser.StringEscapes
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.lexer.MashLexer

object QuoteToggler {

  private val DoubleQuote = "\""

  def toggleQuotes(lineBuffer: LineBuffer, mish: Boolean): LineBuffer = {
    val text = lineBuffer.s
    val cursorPos = lineBuffer.cursorPos

    val tokens = MashLexer.tokenise(text, forgiving = true, includeCommentsAndWhitespace = true, mish = mish)
    var cursorToken = tokens.find(_.region contains cursorPos).getOrElse(tokens.last)
    if (cursorToken.isWhitespace || cursorToken.isEof)
      cursorToken = tokens.find(_.region.posAfter == cursorToken.region.offset).getOrElse(return lineBuffer)
    if (cursorToken.isEof)
      return lineBuffer
    val cursorRegion = cursorToken.region
    val targetRegion = ContiguousRegionFinder.getContiguousRegion(text, cursorRegion, mish)
    val targetText = targetRegion.of(text)
    val isDoubleQuoted = targetText.startsWith(DoubleQuote) && targetText.endsWith(DoubleQuote) && targetText.size > 1
    val isSingleQuoted = targetText.startsWith("'") && targetText.endsWith("'") && targetText.size > 1
    if (isDoubleQuoted || isSingleQuoted)
      unquote(targetText, cursorPos, text, targetRegion)
    else
      quote(targetText, cursorPos, text, targetRegion)

  }

  private def unquote(targetText: String, cursorPos: Int, text: String, region: Region): LineBuffer = {
    val unescaped = StringEscapes.unescape(targetText.tail.init)
    val newText = region.replace(text, unescaped)
    val newCursorPos = if (cursorPos == region.offset) cursorPos else if (region.contains(cursorPos)) cursorPos - 1 else cursorPos - 2
    LineBuffer(newText, newCursorPos)
  }

  private def quote(targetText: String, cursorPos: Int, text: String, targetRegion: Region): LineBuffer = {
    var inner = targetText
    var initialQuotePresent = false
    if (targetText startsWith DoubleQuote) {
      inner = inner.tail
      initialQuotePresent = true
    }
    var finalQuotePresent = false
    if (targetText endsWith DoubleQuote) {
      inner = inner.init
      finalQuotePresent = true
    }
    val escaped = StringEscapes.escapeChars(inner)
    val isSimple = escaped.length == inner.length
    val quoted = DoubleQuote + escaped + DoubleQuote
    val newText = targetRegion.replace(text, quoted)
    val newCursorPos =
      calculateCursorPosAfterQuoting(isSimple, cursorPos, targetRegion, initialQuotePresent, finalQuotePresent, quoted)
    LineBuffer(newText, newCursorPos)
  }

  private def calculateCursorPosAfterQuoting(isSimple: Boolean, cursorPos: Int, replacementRegion: Region, initialQuotePresent: Boolean, finalQuotePresent: Boolean, quoted: String) = {
    if (isSimple) {
      val shift =
        if (cursorPos <= replacementRegion.lastPos)
          if (initialQuotePresent) 0 else 1
        else {
          if (initialQuotePresent || finalQuotePresent) 1 else 2
        }
      cursorPos + shift
    } else // Keeping track of the cursor if there has been escapes is a bit fiddly; for now we shift to the end of the string
      replacementRegion.offset + quoted.length
  }
}