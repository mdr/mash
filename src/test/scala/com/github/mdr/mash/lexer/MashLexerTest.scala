package com.github.mdr.mash.lexer

import com.github.mdr.mash.lexer.TokenType._
import com.github.mdr.mash.parser.MashParserException
import org.scalatest.{ FlatSpec, Matchers }

import scala.collection.mutable.ArrayBuffer

class MashLexerTest extends FlatSpec with Matchers {

  "" shouldProduce Seq()

  "foo" shouldProduce Seq(IDENTIFIER)

  "\"foo\"" shouldProduce Seq(STRING_LITERAL)
  "\"\"" shouldProduce Seq(STRING_LITERAL)
  "'foo'" shouldProduce Seq(STRING_LITERAL)
  "\"foo\nbar\"" shouldProduce Seq(STRING_LITERAL)
  
  "1" shouldProduce Seq(NUMBER_LITERAL)
  "1.0" shouldProduce Seq(NUMBER_LITERAL)
  "0.0" shouldProduce Seq(NUMBER_LITERAL)
  "0" shouldProduce Seq(NUMBER_LITERAL)
  "10" shouldProduce Seq(NUMBER_LITERAL)
  "1234567890" shouldProduce Seq(NUMBER_LITERAL)
  "1.day" shouldProduce Seq(NUMBER_LITERAL, DOT, IDENTIFIER)
  "1." shouldProduce Seq(NUMBER_LITERAL, DOT)
  "-1" shouldProduce Seq(NUMBER_LITERAL)
  "-1.23" shouldProduce Seq(NUMBER_LITERAL)

  "true false" shouldProduce Seq(TRUE, FALSE)

  "ls -a" shouldProduce Seq(IDENTIFIER, SHORT_FLAG)
  "ls --recursive" shouldProduce Seq(IDENTIFIER, LONG_FLAG)
  "foo --bar=baz" shouldProduce Seq(IDENTIFIER, LONG_FLAG, SHORT_EQUALS, IDENTIFIER)

  "a==b" shouldProduce Seq(IDENTIFIER, LONG_EQUALS, IDENTIFIER)
  "a!=b" shouldProduce Seq(IDENTIFIER, NOT_EQUALS, IDENTIFIER)
  "a<b<=c" shouldProduce Seq(IDENTIFIER, LESS_THAN, IDENTIFIER, LESS_THAN_EQUALS, IDENTIFIER)
  "a>b>=c" shouldProduce Seq(IDENTIFIER, GREATER_THAN, IDENTIFIER, GREATER_THAN_EQUALS, IDENTIFIER)

  "_" shouldProduce Seq(HOLE)
  "_a" shouldProduce Seq(IDENTIFIER)
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" shouldProduce Seq(IDENTIFIER)
  "a123457890" shouldProduce Seq(IDENTIFIER)
  "a0b1c2_3" shouldProduce Seq(IDENTIFIER)

  "| => ( )" shouldProduce Seq(PIPE, RIGHT_ARROW, LPAREN, RPAREN)

  "and or" shouldProduce Seq(AND, OR)

  "+ - * /" shouldProduce Seq(PLUS, MINUS, TIMES, DIVIDE)

  "if then else" shouldProduce Seq(IF, THEN, ELSE)

  "def" shouldProduce Seq(DEF)

  "[ ] ," shouldProduce Seq(LSQUARE, RSQUARE, COMMA)

  "{ } :" shouldProduce Seq(LBRACE, RBRACE, COLON)

  "first [1]" shouldProduce Seq(IDENTIFIER, LSQUARE, NUMBER_LITERAL, RSQUARE)

  "first[1]" shouldProduce Seq(IDENTIFIER, LSQUARE_LOOKUP, NUMBER_LITERAL, RSQUARE)

  "_[1]" shouldProduce Seq(HOLE, LSQUARE_LOOKUP, NUMBER_LITERAL, RSQUARE)

  "3[" shouldProduce Seq(NUMBER_LITERAL, LSQUARE_LOOKUP)

  "\"foo\"[" shouldProduce Seq(STRING_LITERAL, LSQUARE_LOOKUP)

  "null[" shouldProduce Seq(NULL, LSQUARE_LOOKUP)

  ")[" shouldProduce Seq(RPAREN, LSQUARE_LOOKUP)

  "][" shouldProduce Seq(RSQUARE, LSQUARE_LOOKUP)

  "}[" shouldProduce Seq(RBRACE, LSQUARE_LOOKUP)

  "?." shouldProduce Seq(DOT_NULL_SAFE)
  "..." shouldProduce Seq(ELLIPSIS)
  "foo?.bar" shouldProduce Seq(IDENTIFIER, DOT_NULL_SAFE, IDENTIFIER)

  """ "Hello $name, how are you?" """ shouldProduce Seq(STRING_START, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, STRING_END)
  """ "Hello $name, how are you $name?" """ shouldProduce Seq(STRING_START, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, STRING_MIDDLE, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, STRING_END)
  """ "Hello $user.name, how are you?" """ shouldProduce Seq(STRING_START, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, DOT, IDENTIFIER, STRING_END)
  """ "Hello ${name}, how are you?" """ shouldProduce Seq(STRING_START, STRING_INTERPOLATION_START_COMPLEX, IDENTIFIER, RBRACE, STRING_END)
  """ "Hello ${3 + 2}, how are you?" """ shouldProduce Seq(STRING_START, STRING_INTERPOLATION_START_COMPLEX, NUMBER_LITERAL, PLUS, NUMBER_LITERAL, RBRACE, STRING_END)
  """ "$name" """ shouldProduce Seq(STRING_START, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, STRING_END)
  """ "$name$bar" """ shouldProduce Seq(STRING_START, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, STRING_END)
  """ "${name}" """ shouldProduce Seq(STRING_START, STRING_INTERPOLATION_START_COMPLEX, IDENTIFIER, RBRACE, STRING_END)
  """ "${name}${bar}" """ shouldProduce Seq(STRING_START, STRING_INTERPOLATION_START_COMPLEX, IDENTIFIER, RBRACE, STRING_INTERPOLATION_START_COMPLEX, IDENTIFIER, RBRACE, STRING_END)
  """ "${{}}" """ shouldProduce Seq(STRING_START, STRING_INTERPOLATION_START_COMPLEX, LBRACE, RBRACE, RBRACE, STRING_END)
  """ "Hello $user.fullName!" """ shouldProduce Seq(STRING_START, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, DOT, IDENTIFIER, STRING_END)
  """ "$_" """ shouldProduce Seq(STRING_START, STRING_INTERPOLATION_START_SIMPLE, HOLE, STRING_END)

  // mish within mash

  "!{ls}" shouldProduce Seq(MISH_INTERPOLATION_START, MISH_WORD, RBRACE)
  "foo !{ls} bar" shouldProduce Seq(IDENTIFIER, MISH_INTERPOLATION_START, MISH_WORD, RBRACE, IDENTIFIER)
  "!{ls ${pwd} }" shouldProduce Seq(MISH_INTERPOLATION_START, MISH_WORD, STRING_INTERPOLATION_START_COMPLEX, IDENTIFIER, RBRACE, RBRACE)
  "!mplayer" shouldProduce Seq(MISH_WORD)
  "!mplayer video" shouldProduce Seq(MISH_WORD, IDENTIFIER)

  "!" shouldProduce Seq(ERROR)

  { // mish mode
    implicit val mode = Mode(mish = true)

    "foo" shouldProduce Seq(MISH_WORD)
    "foo.txt" shouldProduce Seq(MISH_WORD)
    "/bin/bash" shouldProduce Seq(MISH_WORD)
    "git status" shouldProduce Seq(MISH_WORD, MISH_WORD)

    """cd "My Documents" """ shouldProduce Seq(MISH_WORD, STRING_LITERAL)

    " ls $pwd -l" shouldProduce Seq(MISH_WORD, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, MISH_WORD)
    " ls ${pwd} -l" shouldProduce Seq(MISH_WORD, STRING_INTERPOLATION_START_COMPLEX, IDENTIFIER, RBRACE, MISH_WORD)
    """ "Hello $name, how are you?" """ shouldProduce Seq(STRING_START, STRING_INTERPOLATION_START_SIMPLE, IDENTIFIER, STRING_END)

    " 'foo' " shouldProduce Seq(STRING_LITERAL)
    " '$foo' " shouldProduce Seq(STRING_LITERAL)
  }

  { // including whitespace
    implicit val mode = Mode(includeCommentsAndWhitespace = true)

    "# comment" shouldProduce Seq(COMMENT)
    "  # comment" shouldProduce Seq(WHITESPACE, COMMENT)
    "  #" shouldProduce Seq(WHITESPACE, COMMENT)

    "\t\r\n " shouldProduce Seq(WHITESPACE)

  }

  { // Forgiving mish

    implicit val mode = Mode(mish = true, forgiving = true)

    "foo $" shouldProduce Seq(MISH_WORD, STRING_END)
  }

  { // Forgiving mode
    implicit val mode = Mode(forgiving = true)

    "\"unterminated string" shouldProduce Seq(STRING_LITERAL)
    "03" shouldProduce Seq(NUMBER_LITERAL)
    "$" shouldProduce Seq(ERROR)
  }

  { // Strict mode
    implicit val mode = Mode(forgiving = false)
    "\"unterminated string" shouldThrowAnExceptionAtPos 18
    "03" shouldThrowAnExceptionAtPos 1
    "$" shouldThrowAnExceptionAtPos 0
  }

  // Multilines
  """foo
     bar""" shouldProduce Seq(IDENTIFIER, IDENTIFIER)

  """foo #bar
     baz""" shouldProduce Seq(IDENTIFIER, IDENTIFIER)

  implicit class RichString(s: String) {

    def shouldThrowAnExceptionAtPos(pos: Int)(implicit mode: Mode = Mode()) {
      s"Tokenising $s" should ("throw a MashLexerException at position " + pos) in {
        a[MashParserException] should be thrownBy {
          MashLexer.tokenise(s, forgiving = mode.forgiving)
        }
      }
    }

    def shouldProduce(tokens: Seq[TokenType])(implicit mode: Mode = Mode()): Unit = {
      s"Tokenising $s" should (" tokenise to " + tokens.mkString(", ") + " " + mode) in {
        val result = MashLexer.tokenise(s, forgiving = mode.forgiving, mish = mode.mish)
        val actualTokens = if (mode.includeCommentsAndWhitespace) result.rawTokens else result.tokens
        actualTokens.last.tokenType should equal(TokenType.EOF)

        actualTokens.init.map(_.tokenType) should equal(tokens)
        if (mode.includeCommentsAndWhitespace)
          actualTokens.flatMap(_.text).mkString should equal(s)
      }
    }

  }

}

case class Mode(forgiving: Boolean = true, includeCommentsAndWhitespace: Boolean = false, mish: Boolean = false) {

  override def toString: String = {
    val parts = new ArrayBuffer[String]
    if (forgiving) parts += "forgiving"
    if (includeCommentsAndWhitespace) parts += "incl. comments/whitespace"
    if (mish) parts += "mish"
    parts.mkString("(", " ", ")")
  }

}
