package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.TokenType
import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ConcreteSyntax._

trait ObjectParse {
  self: MashParse ⇒

  protected def objectExpr(): Expr = {
    val lbrace = nextToken()
    if (RBRACE) {
      val rbrace = nextToken()
      ObjectExpr(lbrace, None, rbrace)
    } else {
      val firstEntry = objectEntry()
      val entries = safeWhile(COMMA) {
        val comma = nextToken()
        val entry = objectEntry()
        comma -> entry
      }
      val rbrace = consumeRequiredToken("object", RBRACE)
      ObjectExpr(lbrace, Some(ObjectExprContents(firstEntry, entries)), rbrace)
    }
  }

  private def objectEntry(): ObjectEntry =
    if (IDENTIFIER && Set[TokenType](RBRACE, COMMA).contains(lookahead(1))) {
      val field = nextToken()
      ShorthandObjectEntry(field)
    } else {
      val field = suffixExpr()
      val colon = consumeRequiredToken("object", COLON)
      val expr = statementExpr()
      FullObjectEntry(field, colon, expr)
    }

}