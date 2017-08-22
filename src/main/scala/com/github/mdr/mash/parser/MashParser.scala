package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.{ LexerResult, MashLexer }
import com.github.mdr.mash.parser.ConcreteSyntax.Program
import com.github.mdr.mash.utils.PointedRegion

import scala.language.implicitConversions

case class ParseError(message: String, location: PointedRegion)

object MashParser {

  def parse(s: String, mish: Boolean = false): Either[ParseError, Program] =
    try
      Right(parseProgram(s, forgiving = false, mish = mish))
    catch {
      case e: MashParserException ⇒ Left(e.parseError)
    }

  def parseForgiving(s: String, mish: Boolean = false): Program =
    parseProgram(s, forgiving = true, mish = mish)

  private def parseProgram(s: String, forgiving: Boolean = true, mish: Boolean = false): Program = {
    val lexerResult = MashLexer.tokenise(s, forgiving = forgiving, mish = mish)
    parseProgram(lexerResult, forgiving, mish)
  }

  def parseProgram(lexerResult: LexerResult, forgiving: Boolean, mish: Boolean): Program = {
    val parse = new MashParse(lexerResult, initialForgiving = forgiving)
    if (mish)
      Program(None, parse.mishExpr())
    else
      parse.program()
  }

}
