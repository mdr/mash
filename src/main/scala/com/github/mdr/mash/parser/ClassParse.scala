package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ConcreteSyntax._

trait ClassParse {
  self: MashParse ⇒

  protected def classDeclaration(attributesOpt: Option[Attributes] = None): ClassDeclaration = noSemis {
    val classToken = consumeRequiredToken("class", CLASS)
    val firstToken = attributesOpt.flatMap(_.tokens.headOption) getOrElse classToken
    val name = consumeRequiredToken("class", IDENTIFIER)
    semisAllowed {
      val params = classParamList()
      val bodyOpt = if (LBRACE) Some(classBody()) else None
      ClassDeclaration(docComment(firstToken), attributesOpt, classToken, name, params, bodyOpt)
    }
  }

  protected def classParamList(): ParamList = {
    def isParamStart = IDENTIFIER || LPAREN || LSQUARE || HOLE
    val params = safeWhile(isParamStart)(parameter())
    ParamList(params)
  }

  private def classBody(): ClassBody = {
    val lbrace = nextToken()
    val methods = semisAllowed(safeWhile(DEF || AT) {
      val attributesOpt = if (AT) Some(attributes()) else None
      val methodDecl = functionDeclaration(attributesOpt)
      val semiOpt = consumeOptionalToken(SEMI)
      Method(methodDecl, semiOpt)
    })
    val rbrace = consumeRequiredToken("class", RBRACE)
    ClassBody(lbrace, methods, rbrace)
  }


}
