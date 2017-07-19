package com.github.mdr.mash.assist

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.compiler.{ CompilationSettings, CompilationUnit, Compiler }
import com.github.mdr.mash.functions.{ MashFunction, MashMethod }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.Type.UserClass
import com.github.mdr.mash.lexer.{ MashLexer, Token }
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.ConcreteSyntax
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.utils.Utils._
import com.github.mdr.mash.utils.{ Region, Utils }

object InvocationAssistance {

  def getCallingSyntaxOfNearestFunction(s: String,
                                        pos: Int,
                                        bindings: Map[String, MashValue],
                                        mish: Boolean): Option[AssistanceState] =
    for {
      functionType ← getTypeOfNearestFunction(s, pos, bindings, mish)
      assistanceState ← assistInvocation(functionType)
    } yield assistanceState

  private[assist] def getTypeOfNearestFunction(s: String,
                                               pos: Int,
                                               bindings: Map[String, MashValue],
                                               mish: Boolean): Option[Type] = {
    val tokens = MashLexer.tokenise(s, forgiving = true, mish = mish).rawTokens
    val settings = CompilationSettings(inferTypes = true)
    val expr = Compiler.compileForgiving(CompilationUnit(s, mish = mish), bindings, settings)
    for {
      invocationExpr ← findInnermostInvocationContaining(expr, tokens, pos)
      functionType ← getFunctionType(invocationExpr)
    } yield functionType
  }

  private def getFunctionType(expr: Expr): Option[Type] = expr match {
    case InvocationExpr(f, _, _, _) ⇒ f.typeOpt
    case _                          ⇒ expr.preInvocationTypeOpt orElse expr.typeOpt
  }

  private def findInnermostInvocationContaining(program: AstNode, tokens: Seq[Token], pos: Int): Option[Expr] = {
    val enclosingInvocations = program.findAll {
      case expr: InvocationExpr if expandedRegionContains(expr, tokens, pos)                              ⇒ expr
      case expr: Expr if expr.preInvocationTypeOpt.isDefined && expandedRegionContains(expr, tokens, pos) ⇒ expr
      case expr: Expr if hasFunctionType(expr) && expandedRegionContains(expr, tokens, pos)               ⇒ expr
    }
    def size(expr: Expr): Int = expr.sourceInfoOpt.map(_.node.region.length).getOrElse(Integer.MAX_VALUE)
    minBy(enclosingInvocations, size)
  }

  private def hasFunctionType(e: Expr): Boolean =
    e.typeOpt.collect {
      case Type.BuiltinFunction(_) | Type.BoundBuiltinMethod(_, _)      ⇒ true
      case _: Type.UserDefinedFunction | _: Type.BoundUserDefinedMethod ⇒ true
    }.isDefined

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
    // Grow EOF tokens to pick up cursors at the end of the buffer, which is one more than the size of the buffer contents.
    def getRegion(token: Token) = token.region.when(token.isEof, _ grow 1)
    rightmostTokenOpt.map(getRegion) getOrElse Region(0, 0) merge node.region
  }

  private def getTokensAfterLast(lastToken: Token, remainingTokens: Seq[Token]): Seq[Token] =
    Utils.indexOf(remainingTokens, lastToken)
      .map(i => remainingTokens.drop(i + 1))
      .getOrElse(Seq())

  private def findRightmostToken(remainingTokens: Seq[Token]): Option[Token] =
    remainingTokens.takeWhile(token ⇒ token.isWhitespace || token.isComment || token.isEof).lastOption

  private def assistInvocation(functionType: Type): Option[AssistanceState] =
    functionType match {
      case Type.Seq(elementType)                  ⇒ assistInvocation(elementType)
      case Type.BuiltinFunction(f)                ⇒ Some(assistFunction(f))
      case f: Type.UserDefinedFunction            ⇒ Some(assistFunction(f))
      case Type.BoundBuiltinMethod(_, method)     ⇒ Some(assistMethod(method))
      case Type.BoundUserDefinedMethod(_, method) ⇒ Some(assistMethod(method))
      case userClass: Type.UserClass              ⇒ Some(assistClass(userClass))
      case _                                      ⇒ None
      // TODO: Handle .new calls on builtin classes
    }

  private def assistFunction(f: MashFunction): AssistanceState =
    AssistanceState(
      f.name,
      f.summaryOpt.toSeq ++ Seq(
        s"${f.name} ${f.params.callingSyntax}"))

  private def assistFunction(f: Type.UserDefinedFunction) = {
    val Type.UserDefinedFunction(docCommentOpt, _, nameOpt, params, _, _) = f
    AssistanceState(
      nameOpt.getOrElse("Anonymous function"),
      docCommentOpt.map(_.summary).toSeq ++ Seq(
        s"${nameOpt getOrElse "f"} ${params.callingSyntax}"))
  }

  private def assistMethod(method: MashMethod): AssistanceState =
    AssistanceState(
      method.name,
      method.summaryOpt.toSeq ++ Seq(
        s"target.${method.name} ${method.params.callingSyntax}"))

  private def assistMethod(method: Type.UserDefinedFunction): AssistanceState = {
    val Type.UserDefinedFunction(docCommentOpt, _, nameOpt, params, _, _) = method
    AssistanceState(
      nameOpt.getOrElse("Anonymous method"),
      docCommentOpt.map(_.summary).toSeq ++ Seq(
        s"target.${nameOpt getOrElse "method"} ${params.callingSyntax}"))
  }

  private def assistClass(userClass: UserClass): AssistanceState =
    AssistanceState(
      MashClass.ConstructorMethodName,
      Seq(
        s"Construct a new ${userClass.name} object",
        s"${userClass.name}.${MashClass.ConstructorMethodName} ${userClass.params.callingSyntax}"))

}