package com.github.mdr.mash.assist

import scala.PartialFunction._
import com.github.mdr.mash.compiler.Compiler
import com.github.mdr.mash.completions.ContiguousRegionFinder
import com.github.mdr.mash.completions.InvocationFinder
import com.github.mdr.mash.completions.InvocationInfo
import com.github.mdr.mash.evaluator.BoundMethod
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.ConcreteSyntax
import com.github.mdr.mash.parser.SourceInfo
import com.github.mdr.mash.utils.Utils
import com.github.mdr.mash.compiler.CompilationSettings

object InvocationAssistance {

  private def isNearby(pos: Int, token: Token) = token.region.contains(pos) || pos == token.region.posAfter

  def getCallingSyntaxOfCurrentInvocation(s: String, pos: Int, bindings: Map[String, MashValue], mish: Boolean): Option[AssistanceState] = {
    val tokens = MashLexer.tokenise(s, forgiving = true, includeCommentsAndWhitespace = true, mish = mish)
    for {
      expr ← Compiler.compile(CompilationUnit(s, mish = mish), bindings, CompilationSettings(forgiving = true, inferTypes = true))
      sourceInfo ← expr.sourceInfoOpt
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
    case Type.Seq(seqType) ⇒ assistInvocation(seqType)
    case Type.DefinedFunction(f) ⇒
      Some(AssistanceState(
        f.name,
        Seq(
          f.summary,
          "",
          callingSyntax(f))))
    case bm @ Type.BoundMethod(t, m) ⇒
      Some(AssistanceState(
        m.name,
        Seq(
          m.summary,
          "",
          "target." + callingSyntax(m))))
    case _ ⇒ None
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
    def size(expr: Expr): Int = expr.sourceInfoOpt.map(_.expr.region.length).getOrElse(Integer.MAX_VALUE)
    Utils.minBy(enclosingInvocations, size)
  }

  private def hasFunctionType(e: Expr) =
    e.typeOpt.exists(cond(_) {
      case Type.DefinedFunction(_) | Type.BoundMethod(_, _) ⇒ true
    })

  private def expandedRegionContains(expr: Expr, tokens: Seq[Token], pos: Int): Boolean =
    expr.sourceInfoOpt.exists(info ⇒ rightExpandedRegion(info.expr, tokens) contains pos)

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
      case Seq(head, rest @ _*) ⇒
        if (head == lastToken)
          rest
        else
          getTokensAfterLast(lastToken, rest)
      case Seq() ⇒
        Seq()
    }

  private def findRightmostToken(remainingTokens: Seq[Token]): Option[Token] =
    remainingTokens match {
      case Seq(head, rest @ _*) if head.isWhitespace || head.isComment || head.isEof ⇒
        findRightmostToken(rest) orElse Some(head)
      case _ ⇒
        None
    }
}