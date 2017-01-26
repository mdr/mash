package com.github.mdr.mash.editor

import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ConcreteSyntax._
import com.github.mdr.mash.parser.MashParser

object BracketMatcher {

  /**
    * Find a matching bracket (, [ or { for the token at the given offset
    */
  def findMatchingBracket(programText: String, offset: Int, mish: Boolean): Option[Int] = {
    val program = MashParser.parseForgiving(programText, mish = mish)
    for {
      token ← program.tokens find (_.region contains offset)
      matchingBracket ← findMatchingBracket(program, token)
      if matchingBracket.text.nonEmpty // exclude synthetic tokens
      offset = matchingBracket.offset
    } yield matchingBracket.tokenType match {
      case MISH_INTERPOLATION_START            ⇒ offset + 1
      case MISH_INTERPOLATION_START_NO_CAPTURE ⇒ offset + 2
      case STRING_INTERPOLATION_START_COMPLEX  ⇒ offset + 1
      case _                                   ⇒ offset
    }
  }

  private def findMatchingBracket(expr: AstNode, bracket: Token): Option[Token] = expr.find {
    case ParenExpr(`bracket`, _, rparen)              ⇒ rparen
    case ParenExpr(lparen, _, `bracket`)              ⇒ lparen
    case ParenInvocationExpr(_, `bracket`, _, rparen) ⇒ rparen
    case ParenInvocationExpr(_, lparen, _, `bracket`) ⇒ lparen
    case ParenParam(`bracket`, _, _, rparen)          ⇒ rparen
    case ParenParam(lparen, _, _, `bracket`)          ⇒ lparen
    case LookupExpr(_, `bracket`, _, rsquare)         ⇒ rsquare
    case LookupExpr(_, lsquare, __, `bracket`)        ⇒ lsquare
    case ListExpr(`bracket`, _, rsquare)              ⇒ rsquare
    case ListExpr(lsquare, __, `bracket`)             ⇒ lsquare
    case ListPattern(`bracket`, _, rsquare)           ⇒ rsquare
    case ListPattern(lsquare, __, `bracket`)          ⇒ lsquare
    case BlockExpr(`bracket`, _, rbrace)              ⇒ rbrace
    case BlockExpr(lbrace, __, `bracket`)             ⇒ lbrace
    case ObjectExpr(`bracket`, _, rbrace)             ⇒ rbrace
    case ObjectExpr(lbrace, __, `bracket`)            ⇒ lbrace
    case ObjectPattern(`bracket`, _, rbrace)          ⇒ rbrace
    case ObjectPattern(lbrace, __, `bracket`)         ⇒ lbrace
    case ClassBody(`bracket`, _, rbrace)              ⇒ rbrace
    case ClassBody(lbrace, __, `bracket`)             ⇒ lbrace
    case MishInterpolationExpr(`bracket`, _, rbrace)  ⇒ rbrace
    case MishInterpolationExpr(lbrace, __, `bracket`) ⇒ lbrace
    case ComplexInterpolation(`bracket`, _, rbrace)   ⇒ rbrace
    case ComplexInterpolation(lbrace, __, `bracket`)  ⇒ lbrace
  }

}
