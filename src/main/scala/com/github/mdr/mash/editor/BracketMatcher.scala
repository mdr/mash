package com.github.mdr.mash.editor

import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ConcreteSyntax._
import com.github.mdr.mash.parser.MashParser

import scala.PartialFunction.condOpt

object BracketMatcher {

  /**
    * Find a matching bracket at the given offset: (, [ or {
    */
  def findMatchingBracket(programText: String, offset: Int, mish: Boolean): Option[Int] = {
    val program = MashParser.parseForgiving(programText, mish = mish)
    for {
      token ← program.tokens find (_.region contains offset)
      matchingBracket ← findMatchingBracket(program, token)
      if matchingBracket.text.nonEmpty // exclude synthetic tokens
      offset = matchingBracket.offset + internalBracketOffset(matchingBracket)
    } yield offset
  }

  /**
    * Pattern for extracting a pair of matching brackets in the syntax tree
    */
  private object Brackets {

    def unapply(expr: AstNode): Option[(Token, Token)] = condOpt(expr) {
      case ParenExpr(left, _, right)              ⇒ (left, right)
      case ParenInvocationExpr(_, left, _, right) ⇒ (left, right)
      case ParenParam(left, _, _, right)          ⇒ (left, right)
      case LookupExpr(_, left, _, right)          ⇒ (left, right)
      case ListExpr(left, _, right)               ⇒ (left, right)
      case ListPattern(left, _, right)            ⇒ (left, right)
      case BlockExpr(left, _, right)              ⇒ (left, right)
      case ObjectExpr(left, _, right)             ⇒ (left, right)
      case ObjectPattern(left, _, right)          ⇒ (left, right)
      case ClassBody(left, _, right)              ⇒ (left, right)
      case MishInterpolationExpr(left, _, right)  ⇒ (left, right)
      case ComplexInterpolation(left, _, right)   ⇒ (left, right)
    }

  }

  private def findMatchingBracket(expr: AstNode, bracket: Token): Option[Token] = expr.find {
    case Brackets(`bracket`, right) ⇒ right
    case Brackets(left, `bracket`)  ⇒ left
  }

  /**
    * Find the offset of the actual bracket character within a potentially multicharacter bracket token.
    */
  private def internalBracketOffset(matchingBracket: Token): Int =
    matchingBracket.tokenType match {
      case MISH_INTERPOLATION_START            ⇒ 1
      case MISH_INTERPOLATION_START_NO_CAPTURE ⇒ 2
      case STRING_INTERPOLATION_START_COMPLEX  ⇒ 1
      case _                                   ⇒ 0
    }
}
