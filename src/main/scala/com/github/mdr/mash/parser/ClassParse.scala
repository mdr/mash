package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.{ Token, TokenType }
import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ConcreteSyntax._

import scala.collection.mutable.ArrayBuffer

trait ClassParse {
  self: MashParse ⇒

  protected def classDeclaration(): ClassDeclaration = {
    val classToken = consumeRequiredToken("class", CLASS)
    val name = consumeRequiredToken("class", IDENTIFIER)
    val params = classParamList()
    val bodyOpt =
      if (LBRACE) {
        val lbrace = nextToken()
        val methods: ArrayBuffer[Method] = ArrayBuffer()
        while (tokenCanStartAMethod) {
          val methodDecl = functionDeclaration()
          val semiOpt = if (SEMI) Some(nextToken()) else None
          methods += Method(methodDecl, semiOpt)
        }
        val rbrace = consumeRequiredToken("class", RBRACE)
        Some(ClassBody(lbrace, methods, rbrace))
      } else
        None
    ClassDeclaration(docComment(classToken), classToken, name, params, bodyOpt)
  }

  private def tokenCanStartAMethod: Boolean = DEF || AT

}
