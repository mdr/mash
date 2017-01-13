package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.{ Token, TokenType }
import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ConcreteSyntax._

import scala.collection.mutable.ArrayBuffer

trait ClassParse {
  self: MashParse ⇒

  protected def consumeRequiredToken(tokenType: TokenType): Token =
    if (tokenType)
      nextToken()
    else if (forgiving)
      syntheticToken(tokenType)
    else
      errorExpectedToken(TokenNames.getOrElse(tokenType, tokenType.toString))

  private val TokenNames: Map[TokenType, String] = Map(
    IDENTIFIER -> "identifier",
    RBRACE -> "}",
    CLASS -> "class"
  )

  protected def classDeclaration(): ClassDeclaration = {
    val classToken = consumeRequiredToken(CLASS)
    val name = consumeRequiredToken(IDENTIFIER)
    val params = classParamList()
    val bodyOpt =
      if (LBRACE) {
        val lbrace = nextToken()
        val methods: ArrayBuffer[Method] = ArrayBuffer()
        while (DEF) {
          val methodDecl = functionDeclaration()
          val semiOpt = if (SEMI) Some(nextToken()) else None
          methods += Method(methodDecl, semiOpt)
        }
        val rbrace = consumeRequiredToken(RBRACE)
        Some(ClassBody(lbrace, methods, rbrace))
      } else
        None
    ClassDeclaration(docComment(classToken), classToken, name, params, bodyOpt)
  }

}
