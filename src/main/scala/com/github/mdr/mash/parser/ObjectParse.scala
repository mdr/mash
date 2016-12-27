package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.{ Token, TokenType }
import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ConcreteSyntax._

import scala.collection.mutable.ArrayBuffer

trait ObjectParse {
  self: MashParse ⇒

  protected def objectExpr(): Expr = {
    val lbrace = nextToken()
    if (RBRACE) {
      val rbrace = nextToken()
      ObjectExpr(lbrace, None, rbrace)
    } else {
      val firstEntry = objectEntry()
      val entries = ArrayBuffer[(Token, ObjectEntry)]()
      safeWhile(COMMA) {
        val comma = nextToken()
        val entry = objectEntry()
        entries += (comma -> entry)
      }
      val rbrace =
        if (RBRACE)
          nextToken()
        else if (forgiving) {
          val lastExpr = (firstEntry +: entries.map(_._2)).last
          val lastToken = lastExpr.tokens.last
          syntheticToken(RBRACE, lastToken)
        } else
          errorExpectedToken("}")
      ObjectExpr(lbrace, Some(ObjectExprContents(firstEntry, entries)), rbrace)
    }
  }

  private def objectEntry(): ObjectEntry =
    if (IDENTIFIER && Set[TokenType](RBRACE, COMMA).contains(lookahead(1))) {
      val field = nextToken()
      ShorthandObjectEntry(field)
    } else {
      val field = suffixExpr()
      val colon =
        if (COLON)
          nextToken()
        else if (forgiving)
          syntheticToken(COLON)
        else
          errorExpectedToken(":")
      val expr = pipeExpr()
      FullObjectEntry(field, colon, expr)
    }

}