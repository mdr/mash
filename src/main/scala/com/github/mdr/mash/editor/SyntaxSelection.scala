package com.github.mdr.mash.editor

import com.github.mdr.mash.lexer.{ MashLexer, TokenType }
import com.github.mdr.mash.parser.ConcreteSyntax.{ ClassDeclaration, FunctionDeclaration, Program }
import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.repl.LineBuffer
import com.github.mdr.mash.utils.Region

object SyntaxSelection {

  /**
    * @return a strictly larger region that contains the current selection region, if possible, else None
    */
  def expandSelection(lineBuffer: LineBuffer, mish: Boolean = false): Option[Region] = {
    val initialRegion = lineBuffer.selectedOrCursorRegion
    val lexerResult = MashLexer.tokenise(lineBuffer.text, forgiving = true, mish = mish)
    val tokenRegions = lexerResult
      .rawTokens
      .collect { case token if isSelectable(token.tokenType) ⇒ token.region }
    val expr = MashParser.parseForgiving(lineBuffer.text, mish = mish)
    val syntaxRegions = getAllSyntaxTreeRegions(expr)
    (tokenRegions ++ syntaxRegions)
      .filter(_ contains initialRegion)
      .filter(_.length > initialRegion.length)
      .sortBy(_.length)
      .headOption
      .orElse(Some(Region(0, lineBuffer.text.length)))
      .filter(_ != initialRegion)
  }

  private def getAllSyntaxTreeRegions(expr: Program): Seq[Region] =
    expr.findAll {
      case decl: ClassDeclaration    ⇒ decl.regionOpt.map(region ⇒ decl.docCommentOpt.map(_.region merge region) getOrElse region)
      case decl: FunctionDeclaration ⇒ decl.regionOpt.map(region ⇒ decl.docCommentOpt.map(_.region merge region) getOrElse region)
      case node                      ⇒ node.regionOpt
    }.flatten

  private def isSelectable(tokenType: TokenType): Boolean =
    tokenType.isKeyword || tokenType.isComment || tokenType.isIdentifier || tokenType.isFlag

}
