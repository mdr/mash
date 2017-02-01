package com.github.mdr.mash.assist

import com.github.mdr.mash.compiler.{ CompilationSettings, CompilationUnit, Compiler }
import com.github.mdr.mash.evaluator.BoundMethod
import com.github.mdr.mash.functions.{ MashFunction, MashMethod }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.{ MashLexer, Token }
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.ConcreteSyntax
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.utils.{ Region, Utils }

import scala.PartialFunction._

object InvocationAssistance {

  def getCallingSyntaxOfCurrentInvocation(s: String,
                                          pos: Int,
                                          bindings: Map[String, MashValue],
                                          mish: Boolean): Option[AssistanceState] = {
    val tokens = MashLexer.tokenise(s, forgiving = true, mish = mish).rawTokens
    val expr = Compiler.compileForgiving(CompilationUnit(s, mish = mish), bindings, CompilationSettings(inferTypes = true)).body
    for {
      sourceInfo ← expr.sourceInfoOpt
      invocationExpr ← findInnermostInvocationContaining(expr, tokens, pos)
      (functionTypeOpt, valueOpt) = getFunctionType(invocationExpr)
      functionType ← functionTypeOpt
      assistanceState ← assistInvocation(functionType)
    } yield assistanceState
  }

  private def getFunctionType(expr: Expr): (Option[Type], Option[MashValue]) = expr match {
    case InvocationExpr(f, _, _, _) ⇒ (f.typeOpt, f.constantValueOpt)
    case _                          ⇒ (expr.preInvocationTypeOpt orElse expr.typeOpt, None)
  }

  private def assistInvocation(functionType: Type): Option[AssistanceState] = functionType match {
    case Type.Seq(elementType)                              ⇒ assistInvocation(elementType)
    case Type.BuiltinFunction(f)                            ⇒
      Some(AssistanceState(
        f.name,
        f.summaryOpt.toSeq ++ Seq(
          "",
          callingSyntax(f))))
    case Type.UserDefinedFunction(_, nameOpt, params, _, _) ⇒
      Some(AssistanceState(
        nameOpt.getOrElse("Anonymous function"),
        Seq(
          nameOpt.getOrElse("f") + " " + params.callingSyntax)))
    case Type.BoundBuiltinMethod(_, method)                 ⇒
      Some(AssistanceState(
        method.name,
        method.summaryOpt.toSeq ++ Seq(
          "",
          "target." + callingSyntax(method))))
    case Type.BoundUserDefinedMethod(_, functionType)       ⇒
      val Type.UserDefinedFunction(_, nameOpt, params, _, _) = functionType
      Some(AssistanceState(
        nameOpt.getOrElse("Anonymous method"),
        Seq(
          "target." + nameOpt.getOrElse("method") + " " + params.callingSyntax)))
    case _                                                  ⇒ None
  }

  def callingSyntax(funOrMethod: Any): String =
    funOrMethod match {
      case f: MashFunction ⇒ f.name + " " + f.params.callingSyntax
      case bm: BoundMethod ⇒ bm.method.name + " " + bm.method.params.callingSyntax
      case m: MashMethod   ⇒ m.name + " " + m.params.callingSyntax
    }

  private def findInnermostInvocationContaining(expr: Expr, tokens: Seq[Token], pos: Int): Option[Expr] = {
    val enclosingInvocations = expr.findAll {
      case e: InvocationExpr if expandedRegionContains(e, tokens, pos)                           ⇒ e
      case e: Expr if e.preInvocationTypeOpt.isDefined && expandedRegionContains(e, tokens, pos) ⇒ e
      case e: Expr if hasFunctionType(e) && expandedRegionContains(e, tokens, pos)               ⇒ e
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

  private def rightExpandedRegion(expr: ConcreteSyntax.AstNode, tokens: Seq[Token]): Region = {
    val rightmostTokenOpt = expr.tokens.lastOption.map { lastToken ⇒
      val tokensAfterLast = getTokensAfterLast(lastToken, tokens)
      findRightmostToken(tokensAfterLast).getOrElse(lastToken)
    }
    def getRegion(token: Token) = if (token.isEof) token.region.grow(1) else token.region
    rightmostTokenOpt.map(getRegion).getOrElse(Region(0, 0)) merge expr.region
  }

  private def getTokensAfterLast(lastToken: Token, remainingTokens: Seq[Token]): Seq[Token] =
    remainingTokens match {
      case Seq(head, rest@_*) ⇒
        if (head == lastToken)
          rest
        else
          getTokensAfterLast(lastToken, rest)
      case Seq()              ⇒
        Seq()
    }

  private def findRightmostToken(remainingTokens: Seq[Token]): Option[Token] =
    remainingTokens match {
      case Seq(head, rest@_*) if head.isWhitespace || head.isComment || head.isEof ⇒
        findRightmostToken(rest) orElse Some(head)
      case _                                                                       ⇒
        None
    }
}