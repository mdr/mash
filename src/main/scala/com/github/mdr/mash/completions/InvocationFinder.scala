package com.github.mdr.mash.completions

import com.github.mdr.mash.parser.ConcreteSyntax
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.lexer.Token
import scala.PartialFunction.{ cond, condOpt }
import com.github.mdr.mash.parser.SourceInfo

/**
 * @param argPos -- index into the invocationExpr's argument list
 */
case class InvocationInfo(invocationExpr: InvocationExpr, argPos: Int)

object InvocationFinder {

  /**
   * Find the InvocationExpr containing the given literal as an argument
   */
  def findInvocationWithLiteralArg(expr: Expr, literalToken: Token): Option[InvocationInfo] = {
    def isLiteralArg(arg: ConcreteSyntax.AstNode) = cond(arg) {
      case ConcreteSyntax.Literal(`literalToken`) ⇒ true
    }
    expr.find {
      case iexpr @ InvocationExpr(_, _, Some(SourceInfo(ArgSite(args)))) if args.exists(isLiteralArg) ⇒
        InvocationInfo(iexpr, args.indexWhere(isLiteralArg))
    }
  }

  /**
   * Find the InvocationExpr containing the given flag as an argument
   */
  def findInvocationWithFlagArg(expr: Expr, flagToken: Token): Option[InvocationInfo] = {
    def isFlagArg(arg: ConcreteSyntax.AstNode) = cond(arg) {
      case ConcreteSyntax.ShortArg(`flagToken`) | ConcreteSyntax.LongArg(`flagToken`, _) ⇒ true
    }
    expr.find {
      case iexpr @ InvocationExpr(_, _, Some(SourceInfo(ArgSite(args)))) if args.exists(isFlagArg) ⇒
        InvocationInfo(iexpr, args.indexWhere(isFlagArg))
    }
  }

  /**
   * A pattern for extracting arguments from a concrete invocation expression.
   * 
   * PipeExpr's are also matched, as they are the associated concrete syntax for an abstract InvocationExpr derived
   * from a pipe expression.
   */
  private object ArgSite {

    def unapply(expr: ConcreteSyntax.Expr): Option[Seq[ConcreteSyntax.AstNode]] = condOpt(expr) {
      case ConcreteSyntax.InvocationExpr(_, args)                                ⇒ args
      case ConcreteSyntax.PipeExpr(_, _, ConcreteSyntax.InvocationExpr(_, args)) ⇒ args // source corresponding to a desugared pipe
    }

  }

}