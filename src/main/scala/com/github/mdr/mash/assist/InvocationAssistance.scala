package com.github.mdr.mash.assist

import com.github.mdr.mash.classes.BoundMethod
import com.github.mdr.mash.compiler.{ CompilationSettings, CompilationUnit, Compiler }
import com.github.mdr.mash.functions.{ MashFunction, MashMethod }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.{ MashLexer, Token }
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.ConcreteSyntax
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.utils.Utils._
import com.github.mdr.mash.utils.{ Region, Utils }

import scala.PartialFunction._

object InvocationAssistance {

  def getCallingSyntaxOfCurrentInvocation(s: String,
                                          pos: Int,
                                          bindings: Map[String, MashValue],
                                          mish: Boolean): Option[AssistanceState] = {
    val tokens = MashLexer.tokenise(s, forgiving = true, mish = mish).rawTokens
    val expr = Compiler.compileForgiving(CompilationUnit(s, mish = mish), bindings, CompilationSettings(inferTypes = true))
    for {
      invocationExpr ← findInnermostInvocationContaining(expr, tokens, pos)
      functionType ← getFunctionType(invocationExpr)
      assistanceState ← assistInvocation(functionType)
    } yield assistanceState
  }

  private def getFunctionType(expr: Expr): Option[Type] = expr match {
    case InvocationExpr(f, _, _, _) ⇒ f.typeOpt
    case _                          ⇒ expr.preInvocationTypeOpt orElse expr.typeOpt
  }

  private def assistInvocation(functionType: Type): Option[AssistanceState] = functionType match {
    case Type.Seq(elementType)                                             ⇒ assistInvocation(elementType)
    case Type.BuiltinFunction(f)                                           ⇒
      Some(AssistanceState(
        f.name,
        f.summaryOpt.toSeq ++ Seq(
          "",
          callingSyntax(f))))
    case Type.UserDefinedFunction(docCommentOpt, _, nameOpt, params, _, _) ⇒
      Some(AssistanceState(
        nameOpt.getOrElse("Anonymous function"),
        docCommentOpt.map(_.summary).toSeq ++ Seq(
          nameOpt.getOrElse("f") + " " + params.callingSyntax)))
    case Type.BoundBuiltinMethod(_, method)                                ⇒
      Some(AssistanceState(
        method.name,
        method.summaryOpt.toSeq ++ Seq(
          "",
          "target." + callingSyntax(method))))
    case Type.BoundUserDefinedMethod(_, functionType)                      ⇒
      val Type.UserDefinedFunction(docCommentOpt, _, nameOpt, params, _, _) = functionType
      Some(AssistanceState(
        nameOpt.getOrElse("Anonymous method"),
        docCommentOpt.map(_.summary).toSeq ++ Seq(
          "target." + nameOpt.getOrElse("method") + " " + params.callingSyntax)))
    case _                                                                 ⇒ None
  }

  private def callingSyntax(funOrMethod: Any): String =
    funOrMethod match {
      case f: MashFunction ⇒ f.name + " " + f.params.callingSyntax
      case bm: BoundMethod ⇒ bm.method.name + " " + bm.method.params.callingSyntax
      case m: MashMethod   ⇒ m.name + " " + m.params.callingSyntax
    }

  private def findInnermostInvocationContaining(program: AstNode, tokens: Seq[Token], pos: Int): Option[Expr] = {
    val enclosingInvocations = program.findAll {
      case expr: InvocationExpr if expandedRegionContains(expr, tokens, pos)                              ⇒ expr
      case expr: Expr if expr.preInvocationTypeOpt.isDefined && expandedRegionContains(expr, tokens, pos) ⇒ expr
      case expr: Expr if hasFunctionType(expr) && expandedRegionContains(expr, tokens, pos)               ⇒ expr
    }
    def size(expr: Expr): Int = expr.sourceInfoOpt.map(_.node.region.length).getOrElse(Integer.MAX_VALUE)
    Utils.minBy(enclosingInvocations, size)
  }

  private def hasFunctionType(e: Expr): Boolean =
    e.typeOpt.exists(cond(_) {
      case Type.BuiltinFunction(_) | Type.BoundBuiltinMethod(_, _)
           | _: Type.UserDefinedFunction | _: Type.BoundUserDefinedMethod ⇒ true
    })

  private def expandedRegionContains(expr: Expr, tokens: Seq[Token], pos: Int): Boolean =
    expr.sourceInfoOpt.exists(info ⇒ rightExpandedRegion(info.node, tokens) contains pos)

  /**
    * Expand the region of the node to include any whitespace / comment / EOF tokens to the right of it.
    */
  private def rightExpandedRegion(node: ConcreteSyntax.AstNode, allTokens: Seq[Token]): Region = {
    val rightmostTokenOpt = node.tokens.lastOption.map { lastToken ⇒
      val tokensAfterLast = getTokensAfterLast(lastToken, allTokens)
      findRightmostToken(tokensAfterLast).getOrElse(lastToken)
    }
    def getRegion(token: Token) = token.region.when(token.isEof, _ grow 1)
    rightmostTokenOpt.map(getRegion) getOrElse Region(0, 0) merge node.region
  }

  private def getTokensAfterLast(lastToken: Token, remainingTokens: Seq[Token]): Seq[Token] =
    Utils.indexOf(remainingTokens, lastToken)
      .map(i => remainingTokens.drop(i + 1))
      .getOrElse(Seq())

  private def findRightmostToken(remainingTokens: Seq[Token]): Option[Token] =
    remainingTokens.takeWhile(token ⇒ token.isWhitespace || token.isComment || token.isEof).headOption

}