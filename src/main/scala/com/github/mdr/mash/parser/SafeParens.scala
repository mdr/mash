package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.TokenType
import com.github.mdr.mash.lexer.TokenType.{ DIVIDE, MINUS, PLUS, TIMES }
import com.github.mdr.mash.parser.ConcreteSyntax._
import com.github.mdr.mash.utils.Utils._

object SafeParens {

  /**
    * Add parentheses to the given expression if required to allow it to be followed either by a lookup or member expression:
    * XXX[1], or XXX.field
    */
  def safeParens(exprString: String): String = {
    val expr = MashParser.parseForgiving(exprString).body
    val needsParens = expr match {
      case _: ParenExpr | _: MemberExpr | _: LookupExpr | _: BlockExpr | _: ListExpr | _: ObjectExpr | _: Identifier ⇒ false
      case _                                                                                                         ⇒ true
    }
    exprString.when(needsParens, parenthesise)
  }

  def safeParens(prefix: String, suffix: String): String = {
    val prefixExpr = MashParser.parseForgiving(prefix).body
    val suffixExpr = MashParser.parseForgiving("it" + suffix).body
    val needsParens = (prefixExpr, suffixExpr) match {
      case (op1: BinOpExpr, op2: BinOpExpr)                                                                                            ⇒
        val token1 = op1.op.tokenType
        val token2 = op2.op.tokenType
        !(isMultiplicativeOp(token1) && isAdditiveOpt(token2) ||
          isMultiplicativeOp(token1) && isMultiplicativeOp(token2) ||
          isAdditiveOpt(token1) && isAdditiveOpt(token2))
      case (_, _: InvocationExpr)                                                                                                      ⇒ true
      case (_: ParenExpr | _: MemberExpr | _: LookupExpr | _: BlockExpr | _: ListExpr | _: ObjectExpr | _: Identifier | _: Literal, _) ⇒ false
      case (_: LambdaExpr, _: PipeExpr)                                                                                                ⇒ true
      case (_, _: PipeExpr)                                                                                                            ⇒ false
      case _                                                                                                                           ⇒ true
    }
    prefix.when(needsParens, parenthesise) + suffix
  }

  private def isAdditiveOpt(tokenType: TokenType) = Set[TokenType](PLUS, MINUS) contains tokenType

  private def isMultiplicativeOp(tokenType: TokenType) = Set[TokenType](TIMES, DIVIDE) contains tokenType

  private def parenthesise(s: String) = s"($s)"

}
