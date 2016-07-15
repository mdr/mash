package com.github.mdr.mash.parser

import scala.language.implicitConversions

import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.parser.ConcreteSyntax.Expr
import com.github.mdr.mash.utils.PointedRegion

case class ParseError(message: String, location: PointedRegion)

object MashParser {

  def parse(s: String, mish: Boolean = false): Either[ParseError, Expr] =
    try
      Right(parseProgram(s, forgiving = false, mish = mish))
    catch {
      case e: MashParserException ⇒ Left(e.parseError)
    }

  def parseForgiving(s: String, mish: Boolean = false): Expr =
    parseProgram(s, forgiving = true, mish = mish)

  private def parseProgram(s: String, forgiving: Boolean = true, mish: Boolean = false): Expr = {
    val lexerResult = MashLexer.tokenise(s, forgiving = forgiving, mish = mish)
    val parse = new MashParse(lexerResult, initialForgiving = forgiving)
    if (mish)
      parse.mishExpr()
    else
      parse.program()
  }

}
