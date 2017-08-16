package com.github.mdr.mash.editor

import com.github.mdr.mash.lexer.{ MashLexer, TokenType }
import com.github.mdr.mash.parser.ConcreteSyntax.{ ClassDeclaration, FunctionDeclaration }
import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.repl.LineBuffer
import com.github.mdr.mash.utils.Region

object SyntaxSelection {

  def expandSelection(lineBuffer: LineBuffer, mish: Boolean = false): Option[Region] = {
    val initialRegion = lineBuffer.selectedOrCursorRegion
    val lexerResult = MashLexer.tokenise(lineBuffer.text, forgiving = true, mish = mish)
    val tokenRegions = lexerResult
      .rawTokens
      .collect { case token if isSelectable(token.tokenType) ⇒ token.region }
    val expr = MashParser.parseForgiving(lineBuffer.text, mish = mish)
    val astRegions = expr.findAll {
      case decl: ClassDeclaration    ⇒ decl.docCommentOpt.map(_.region merge decl.region) getOrElse decl.region
      case decl: FunctionDeclaration ⇒ decl.docCommentOpt.map(_.region merge decl.region) getOrElse decl.region
      case node                      ⇒ node.region
    }
    (tokenRegions ++ astRegions)
      .filter(_ contains initialRegion)
      .filter(_.length > initialRegion.length)
      .sortBy(_.length)
      .headOption
      .orElse(Some(Region(0, lineBuffer.text.length)))
      .filter(_ != initialRegion)
  }

  private def isSelectable(tokenType: TokenType): Boolean =
    tokenType.isKeyword || tokenType.isComment || tokenType.isIdentifier || tokenType.isFlag

}
