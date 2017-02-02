package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ConcreteSyntax._

trait ClassParse {
  self: MashParse ⇒

  protected def classDeclaration(): ClassDeclaration = {
    val classToken = consumeRequiredToken("class", CLASS)
    val name = consumeRequiredToken("class", IDENTIFIER)
    val params = classParamList()
    val bodyOpt = if (LBRACE) Some(classBody()) else None
    ClassDeclaration(docComment(classToken), attributesOpt = None, classToken, name, params, bodyOpt)
  }

  protected def classParamList(): ParamList = {
    def isParamStart = IDENTIFIER || LPAREN || LSQUARE || HOLE
    val params = safeWhile(isParamStart)(parameter())
    ParamList(params)
  }

  private def classBody(): ClassBody = {
    val lbrace = nextToken()
    val methods = semisAllowed(safeWhile(DEF || AT) {
      val methodDecl = functionDeclaration()
      val semiOpt = consumeOptionalToken(SEMI)
      Method(methodDecl, semiOpt)
    })
    val rbrace = consumeRequiredToken("class", RBRACE)
    ClassBody(lbrace, methods, rbrace)
  }


}
