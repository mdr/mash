package com.github.mdr.mash.completions

import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser._

/**
 * Find the comparison type of equality (or inequality) expression containing the given token
 */
object EqualityFinder {

  def findEqualityExprWithLiteralArg(expr: Expr, literalToken: Token): Option[Type] =
    (expr.find {
      case BinOpExpr(Literal(_, Some(SourceInfo(ConcreteSyntax.Literal(`literalToken`)))), BinaryOperator.Equals | BinaryOperator.NotEquals, right, _) ⇒
        right.typeOpt
      case BinOpExpr(StringLiteral(_, _, _, Some(SourceInfo(ConcreteSyntax.Literal(`literalToken`)))), BinaryOperator.Equals | BinaryOperator.NotEquals, right, _) ⇒
        right.typeOpt
      case BinOpExpr(left, BinaryOperator.Equals | BinaryOperator.NotEquals, Literal(_, Some(SourceInfo(ConcreteSyntax.Literal(`literalToken`)))), _) ⇒
        left.typeOpt
      case BinOpExpr(left, BinaryOperator.Equals | BinaryOperator.NotEquals, StringLiteral(_, _, _, Some(SourceInfo(ConcreteSyntax.Literal(`literalToken`)))), _) ⇒
        left.typeOpt
    }).flatten

}