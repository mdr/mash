package com.github.mdr.mash.parser

import scala.collection.mutable.ArrayBuffer
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.ConcreteSyntax._

trait FunctionParse { self: MashParse ⇒

  protected def functionDeclaration(): FunctionDeclaration = {
    val defToken = nextToken()
    val name =
      if (IDENTIFIER)
        nextToken()
      else if (forgiving)
        syntheticToken(IDENTIFIER, defToken)
      else
        errorExpectedToken("identifier")
    val params = paramList()
    val equals =
      if (SHORT_EQUALS)
        nextToken()
      else if (forgiving)
        syntheticToken(SHORT_EQUALS, params.params.lastOption.map(_.tokens.last).getOrElse(name))
      else
        errorExpectedToken("=")
    val body = pipeExpr()
    FunctionDeclaration(defToken, name, params, equals, body)
  }

  private def paramList(): ParamList = {
    val params = ArrayBuffer[FunctionParam]()
    safeWhile(IDENTIFIER || LPAREN) {
      params += parameter()
    }
    ParamList(params)
  }

  private def parameter(): FunctionParam =
    if (IDENTIFIER) {
      val ident = nextToken()
      if (ELLIPSIS) {
        val ellipsis = nextToken()
        VariadicParam(ident, ellipsis)
      } else
        SimpleParam(ident)
    } else if (LPAREN) {
      val lparen = nextToken()
      val param = parameter()
      val actualParam =
        param match {
          case SimpleParam(name) ⇒
            if (SHORT_EQUALS && param.isInstanceOf[SimpleParam]) {
              val equals = nextToken()
              val defaultExpr = pipeExpr()
              DefaultParam(name, equals, defaultExpr)
            } else
              param
          case _ ⇒
            param
        }
      val rparen =
        if (RPAREN)
          nextToken()
        else if (forgiving)
          syntheticToken(RPAREN)
        else
          errorExpectedToken(")")
      ParenParam(lparen, actualParam, rparen)
    } else if (forgiving)
      SimpleParam(syntheticToken(IDENTIFIER))
    else
      errorExpectedToken("identifier")

  private case class LambdaStart(paramList: ParamList, arrow: Token)

  private def lambdaStart(): LambdaStart = {
    val params = paramList()
    val arrow =
      if (RIGHT_ARROW)
        nextToken()
      else if (forgiving)
        syntheticToken(RIGHT_ARROW)
      else
        errorExpectedToken("=>")
    LambdaStart(params, arrow)
  }

  protected def lambdaExpr(mayContainPipe: Boolean = false): Expr = speculate(lambdaStart()) match {
    case Some(LambdaStart(params, arrow)) ⇒
      val body = if (mayContainPipe) pipeExpr() else lambdaExpr(mayContainPipe = false)
      LambdaExpr(params, arrow, body)
    case None ⇒
      assignmentExpr()
  }
}