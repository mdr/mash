package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.TokenType
import com.github.mdr.mash.parser.ConcreteSyntax._
import com.github.mdr.mash.parser.MashParser.parseForgiving
import com.github.mdr.mash.utils.NumberUtils.isInt

import scala.PartialFunction.condOpt

object LookupDecomposer {

  case class LookupWithIntegerIndex(target: String, index: Int)

  case class LookupWithStringIndex(target: String, index: String)

  def decomposeLookupWithIntegerIndex(expr: String): Option[LookupWithIntegerIndex] =
    condOpt(parseForgiving(expr).body) {
      case LookupExpr(targetExpr, _, Literal(token), _) if token.tokenType == TokenType.NUMBER_LITERAL && isInt(token.text) ⇒
        LookupWithIntegerIndex(targetExpr.region.of(expr), token.text.toInt)
    }

  def decomposeLookupWithStringIndex(expr: String): Option[LookupWithStringIndex] =
    condOpt(parseForgiving(expr).body) {
      case LookupExpr(targetExpr, _, Literal(token), _) if token.tokenType == TokenType.STRING_LITERAL ⇒
        val stringLiteral = new Abstractifier(Provenance("internal", expr)).abstractifyStringLiteral(token)
        LookupWithStringIndex(targetExpr.region.of(expr), stringLiteral.s)
    }

  def decomposeMember(expr: String): Option[LookupWithStringIndex] =
    condOpt(parseForgiving(expr).body) {
      case MemberExpr(targetExpr, _, name) ⇒ LookupWithStringIndex(targetExpr.region.of(expr), name.text)
    }

}
