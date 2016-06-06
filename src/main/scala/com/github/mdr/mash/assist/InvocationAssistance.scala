package com.github.mdr.mash.assist

import scala.PartialFunction.condOpt

import com.github.mdr.mash.compiler.Compiler
import com.github.mdr.mash.completions.ContiguousRegionFinder
import com.github.mdr.mash.completions.InvocationFinder
import com.github.mdr.mash.completions.InvocationInfo
import com.github.mdr.mash.evaluator.BoundMethod
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils

object InvocationAssistance {

  private def isNearby(pos: Int, token: Token) = token.region.contains(pos) || pos == token.region.posAfter

  def getCallingSyntaxOfCurrentInvocation(s: String, pos: Int, env: Environment, mish: Boolean): Option[AssistanceState] = {
    val tokens = MashLexer.tokenise(s, forgiving = true, includeCommentsAndWhitespace = true, mish = mish)
    tokens.find(isNearby(pos, _)).flatMap { nearbyToken ⇒
      val region = ContiguousRegionFinder.getContiguousRegion(s, nearbyToken.region, mish = mish, liberal = true)
      val replacement = "\"" + region.of(s).filterNot('"' == _) + "\""
      val replaced = StringUtils.replace(s, region, replacement)
      val exprOpt = Compiler.compile(replaced, env, forgiving = true, inferTypes = true, mish = mish)
      def isNearbyStringToken(token: Token) = token.isString && isNearby(region.offset, token)
      for {
        expr ← exprOpt
        sourceInfo ← expr.sourceInfoOpt
        tokens = sourceInfo.expr.tokens
        literalToken ← tokens.find(isNearbyStringToken)
        InvocationInfo(invocationExpr, _) ← InvocationFinder.findInvocationWithLiteralArg(expr, literalToken)
        functionType ← invocationExpr.function.typeOpt
        assistanceState ← assistInvocation(functionType)
      } yield assistanceState
    } orElse {
      val region = Region(pos, 0)
      val replacement = "\"" + region.of(s).filterNot('"' == _) + "\""
      val replaced = StringUtils.replace(s, region, replacement)
      val exprOpt = Compiler.compile(replaced, env, forgiving = true, inferTypes = true, mish = mish)
      def isNearbyStringToken(token: Token) = token.isString && isNearby(region.offset, token)
      for {
        expr ← exprOpt
        sourceInfo ← expr.sourceInfoOpt
        tokens = sourceInfo.expr.tokens
        literalToken ← tokens.find(isNearbyStringToken)
        InvocationInfo(invocationExpr, _) ← InvocationFinder.findInvocationWithLiteralArg(expr, literalToken)
        functionType ← invocationExpr.function.typeOpt
        assistanceState ← assistInvocation(functionType)
      } yield assistanceState

    }
  }

  private def assistInvocation(functionType: Type): Option[AssistanceState] = condOpt(functionType) {
    case Type.DefinedFunction(f) ⇒
      AssistanceState(
        f.name,
        Seq(
          f.summary,
          "",
          callingSyntax(f)))
    case bm @ Type.BoundMethod(t, m) ⇒
      AssistanceState(
        m.name,
        Seq(
          m.summary,
          "",
          "target." + callingSyntax(m)))
  }

  def callingSyntax(funOrMethod: Any): String =
    funOrMethod match {
      case f: MashFunction ⇒ f.name + " " + f.params.callingSyntax
      case bm: BoundMethod ⇒ bm.method.name + " " + bm.method.params.callingSyntax
      case m: MashMethod   ⇒ m.name + " " + m.params.callingSyntax
    }
}