package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.{ Token, TokenType }
import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ConcreteSyntax._

trait ClassParse {
  self: MashParse ⇒

  protected def consumeToken(tokenType: TokenType): Token =
    if (tokenType)
      nextToken()
    else if (forgiving)
      syntheticToken(tokenType)
    else
      errorExpectedToken(TokenNames.getOrElse(tokenType, tokenType.toString))

  private val TokenNames: Map[TokenType, String] = Map(
    IDENTIFIER -> "identifier",
    CLASS -> "class"
  )

  protected def classDeclaration(): ClassDeclaration = {
    val classToken = consumeToken(CLASS)
    val name = consumeToken(IDENTIFIER)
    val params = paramList()
    ClassDeclaration(classToken, name, params)
  }

}
