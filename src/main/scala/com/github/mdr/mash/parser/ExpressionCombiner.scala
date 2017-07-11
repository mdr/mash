package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.TokenType
import com.github.mdr.mash.lexer.TokenType.{ DIVIDE, MINUS, PLUS, TIMES }
import com.github.mdr.mash.parser.ConcreteSyntax._
import com.github.mdr.mash.utils.Utils._

object ExpressionCombiner {

  /**
    * Add parentheses around the given prefix expression if required to allow it to be safely followed by
    * the given suffix and combine in the expected way.
    *
    * For example: safeParens("1 + 2", " * 3") ==> "(1 + 2) * 3"
    */
  def combineSafely(prefix: String, suffix: String): String = {
    val prefixExpr = MashParser.parseForgiving(prefix).body
    val suffixExpr = MashParser.parseForgiving("it" + suffix).body
    val needsParens = (prefixExpr, suffixExpr) match {
      case (prefixOp: BinOpExpr, suffixOp: BinOpExpr)                    ⇒ needsParensBinOp(prefixOp, suffixOp)
      case (_, _: InvocationExpr)                                        ⇒ true
      case (_: ParenExpr | _: MemberExpr | _: ParenInvocationExpr | _: LookupExpr | _: BlockExpr |
            _: ListExpr | _: ObjectExpr | _: Identifier | _: Literal, _) ⇒ false
      case (_: LambdaExpr, _: PipeExpr)                                  ⇒ true
      case (_, _: PipeExpr)                                              ⇒ false
      case _                                                             ⇒ true
    }
    prefix.when(needsParens, parenthesise) + suffix
  }

  private def needsParensBinOp(prefixOp: BinOpExpr, suffixOp: BinOpExpr): Boolean = {
    val token1 = prefixOp.op.tokenType
    val token2 = suffixOp.op.tokenType
    !(isMultiplicativeOp(token1) && isAdditiveOp(token2) ||
      isMultiplicativeOp(token1) && isMultiplicativeOp(token2) ||
      isAdditiveOp(token1) && isAdditiveOp(token2))
  }

  private def isAdditiveOp(tokenType: TokenType) = Set[TokenType](PLUS, MINUS) contains tokenType

  private def isMultiplicativeOp(tokenType: TokenType) = Set[TokenType](TIMES, DIVIDE) contains tokenType

  private def parenthesise(s: String) = s"($s)"

}
