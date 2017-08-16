package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.TokenType
import com.github.mdr.mash.parser.ConcreteSyntax._
import com.github.mdr.mash.utils.NumberUtils.isInt

object LookupDecomposer {

  case class LookupWithIntegerIndex(target: String, index: Int)

  case class LookupWithStringIndex(target: String, index: String)

  def decomposeLookupWithIntegerIndex(expr: String): Option[LookupWithIntegerIndex] =
    parse(expr).collect {
      case LookupExpr(targetExpr, _, Literal(token), _) if token.tokenType == TokenType.NUMBER_LITERAL && isInt(token.text) ⇒
        targetExpr.regionOpt.map(region ⇒ LookupWithIntegerIndex(region.of(expr), token.text.toInt))
    }.flatten

  def decomposeLookupWithStringIndex(expr: String): Option[LookupWithStringIndex] =
    parse(expr).collect {
      case LookupExpr(targetExpr, _, Literal(token), _) if token.tokenType == TokenType.STRING_LITERAL ⇒
        val stringLiteral = new Abstractifier(Provenance("internal", expr)).abstractifyStringLiteral(token)
        targetExpr.regionOpt.map(region ⇒ LookupWithStringIndex(region.of(expr), stringLiteral.s))
    }.flatten

  def decomposeMember(expr: String): Option[LookupWithStringIndex] =
    parse(expr).collect {
      case MemberExpr(targetExpr, _, name) ⇒
        targetExpr.regionOpt.map(region ⇒ LookupWithStringIndex(region.of(expr), name.text))
    }.flatten

  private def parse(expr: String): Option[Expr] = MashParser.parse(expr).right.toOption.map(_.body)

}
