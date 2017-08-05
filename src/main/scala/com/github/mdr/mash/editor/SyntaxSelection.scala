package com.github.mdr.mash.editor

import com.github.mdr.mash.lexer.{ MashLexer, TokenType }
import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.repl.LineBuffer
import com.github.mdr.mash.utils.Region

object SyntaxSelection {

  def expandSelection(lineBuffer: LineBuffer): LineBuffer = {
    val initialRegion = lineBuffer.selectedOrCursorRegion
    val tokenRegions = MashLexer.tokenise(lineBuffer.text, forgiving = true)
      .rawTokens
      .collect { case token if isSelectable(token.tokenType) ⇒ token.region }
    val expr = MashParser.parseForgiving(lineBuffer.text).body
    val astRegions = expr.findAllMatching(_ ⇒ true).map(_.region)
    val nextBiggestRegion: Region =
      (tokenRegions ++ astRegions)
        .filter(_ contains initialRegion)
        .filter(_.length > initialRegion.length)
        .sortBy(_.length)
        .headOption
        .getOrElse(Region(0, lineBuffer.text.length))
    expandSelection(lineBuffer, nextBiggestRegion)
  }

  private def isSelectable(tokenType: TokenType): Boolean =
    tokenType.isKeyword || tokenType.isComment || tokenType.isIdentifier || tokenType.isFlag

  private def expandSelection(lineBuffer: LineBuffer, newSelectionRegion: Region): LineBuffer = {
    val newCursorOffset = newSelectionRegion.posAfter
    val newSelectionOffsetOpt = Some(newSelectionRegion.offset)
    LineBuffer(lineBuffer.text, newCursorOffset, newSelectionOffsetOpt)
  }

}
