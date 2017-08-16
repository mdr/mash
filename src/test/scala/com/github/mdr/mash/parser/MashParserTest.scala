package com.github.mdr.mash.parser

import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.parser.ConcreteSyntax._
import com.github.mdr.mash.utils.Region
import org.scalatest.{ FlatSpec, Matchers }

class MashParserTest extends FlatSpec with Matchers {

  private def parse(s: String) = MashParser.parseForgiving(s).body

  "Parsing inequality" should "work" in {
    val s = "10 != 20"
    val Seq(ten, notEquals, twenty, eof) = MashLexer.tokenise(s).tokens
    parse(s) should equal(BinOpExpr(Literal(ten), notEquals, Literal(twenty)))
  }

  "Parsing pipes" should "be left associative" in {
    val s = "a | b | c"
    val Seq(a, pipe1, b, pipe2, c, eof) = MashLexer.tokenise(s).tokens
    // (a | b) | c
    parse(s) should equal(
      PipeExpr(
        PipeExpr(
          Identifier(a),
          pipe1,
          Identifier(b)),
        pipe2,
        Identifier(c)))
  }

  "A lambda on the left of a pipe" should "bind looser than pipes" in {
    val s = "x => x | a"
    val Seq(x, arrow, x2, pipe, a, eof) = MashLexer.tokenise(s).tokens
    // x ⇒ (x | a)
    parse(s) should equal(
      LambdaExpr(Some(ParamList(Seq(SimpleParam(IdentPattern(x))))), arrow,
        PipeExpr(
          Identifier(x2),
          pipe,
          Identifier(a))))
  }

  "A lambda on the left of a pipe" should "bind looser than pipes when nested" in {
    val s = "x => y => y | length"
    val Seq(x, arr, y, arr2, y2, pipe, length, _) = MashLexer.tokenise(s).tokens
    // (x ⇒ y ⇒ y) | length
    parse(s) should equal(
      LambdaExpr(
        Some(ParamList(Seq(SimpleParam(IdentPattern(x))))),
        arr,
        LambdaExpr(
          Some(ParamList(Seq(SimpleParam(IdentPattern(y))))),
          arr2,
          PipeExpr(
            Identifier(y2),
            pipe,
            Identifier(length)))))
  }

  "A lambda on the right of a pipe" should "bind tighter than pipes" in {
    val s = "a | x => x | b "
    val Seq(a, pipe, x, arrow, x2, pipe2, b, eof) = MashLexer.tokenise(s).tokens
    // a | (x ⇒ x) | b
    parse(s) should equal(
      PipeExpr(
        PipeExpr(
          Identifier(a),
          pipe,
          LambdaExpr(Some(ParamList(Seq(SimpleParam(IdentPattern(x))))), arrow,
            Identifier(x2))),
        pipe2,
        Identifier(b)))
  }

  "A lambda on the right of a pipe" should "bind tighter than pipes when nested" in {
    val s = "a | x => y => y | b"
    val Seq(a, pipe, x, arr, y, arr2, y2, pipe2, b, _) = MashLexer.tokenise(s).tokens

    // a | (x ⇒ y ⇒ y) | b
    parse(s) should equal(
      PipeExpr(
        PipeExpr(
          Identifier(a),
          pipe,
          LambdaExpr(Some(ParamList(Seq(SimpleParam(IdentPattern(x))))), arr,
            LambdaExpr(Some(ParamList(Seq(SimpleParam(IdentPattern(y))))), arr2,
              Identifier(y2)))),
        pipe2,
        Identifier(b)))
  }

  "Less than operators" should "be chainable" in {
    val s = "0 <= x < y <= 100"
    val Seq(zero, leq1, x, lt, y, leq2, hundred, eof) = MashLexer.tokenise(s).tokens
    parse(s) should equal(
      ChainedOpExpr(Literal(zero), Seq(
        (leq1, Identifier(x)),
        (lt, Identifier(y)),
        (leq2, Literal(hundred)))))
  }

  "Greater than operators" should "be chainable" in {
    val s = "100 >= x > y >= 0"
    val Seq(hundred, geq1, x, gt, y, geq2, zero, eof) = MashLexer.tokenise(s).tokens
    parse(s) should equal(
      ChainedOpExpr(Literal(hundred), Seq(
        (geq1, Identifier(x)),
        (gt, Identifier(y)),
        (geq2, Literal(zero)))))
  }

  "and" should "bind tighter than or" in {
    val s = "a and b or c"
    val Seq(a, and, b, or, c, _) = MashLexer.tokenise(s).tokens
    // (a and b) or c
    parse(s) should equal(
      BinOpExpr(
        BinOpExpr(
          Identifier(a),
          and,
          Identifier(b)),
        or,
        Identifier(c)))
  }

  "invocations" should "bind tighter than comparison operators" in {
    val s = "length xs > 5"
    val Seq(length, xs, gt, five, _) = MashLexer.tokenise(s).tokens
    // (length xs) > 5
    parse(s) should equal(
      BinOpExpr(
        InvocationExpr(
          Identifier(length),
          Seq(
            Identifier(xs))),
        gt,
        Literal(five)))
  }

  "Long flag arguments" should "work" in {
    val s = "a --b=c.d"
    val Seq(a, b, eq, c, dot, d, _) = MashLexer.tokenise(s).tokens
    parse(s) should equal(
      InvocationExpr(
        Identifier(a),
        Seq(
          LongArg(
            b,
            Some(eq,
              MemberExpr(
                Identifier(c), dot, d))))))

  }

  "Parser" should "recognise lookup expressions" in {
    val s = "a[0]"
    val Seq(a, lsquare, zero, rsquare, _) = MashLexer.tokenise(s).tokens
    parse(s) should equal(
      LookupExpr(
        Identifier(a),
        lsquare,
        Literal(zero),
        rsquare))
  }

  it should "fail to parse a keyword within an interpolation" in {
    MashParser.parse(""" "$if" """) should be('left)
  }

  "Forgiving parser" should "not enter an infinite loop" in {
    val s = """["foo", 2}."""
    MashParser.parseForgiving(s)
  }

  it should "not enter an infinite loop here either" in {
    val s = """{"rss": _.r"""
    MashParser.parseForgiving(s)
  }

  it should "not enter an infinite loop here either (2)" in {
    val s = "ls | where (_.owner = mat"
    MashParser.parseForgiving(s)
  }

  "Parsing interpolated strings" should "parse simple variables" in {
    val s = """ "Hello $name!" """
    val Seq(start, dollar, name, end, _) = MashLexer.tokenise(s).tokens
    parse(s) shouldEqual
      InterpolatedString(start, Seq(SimpleInterpolation(dollar, Identifier(name))), end)
  }

  it should "parse dot sequences" in {
    val s = """ "Hello $user.name!" """
    val Seq(start, dollar, user, dot, name, end, _) = MashLexer.tokenise(s).tokens
    parse(s) shouldEqual
      InterpolatedString(start, Seq(SimpleInterpolation(dollar, MemberExpr(Identifier(user), dot, name))), end)
  }

  it should "parse brace sequences" in {
    val s = """ "Hello ${name}!" """
    val Seq(start, interpStart, name, rbrace, end, _) = MashLexer.tokenise(s).tokens
    parse(s) shouldEqual
      InterpolatedString(start, Seq(ComplexInterpolation(interpStart, Identifier(name), rbrace)), end)
  }

  it should "parse multiple sections" in {
    val s = """ "Hello $name! You are in $pwd!" """
    val Seq(start, dollar, name, middle, dollar2, pwd, end, _) = MashLexer.tokenise(s).tokens
    parse(s) shouldEqual
      InterpolatedString(start, Seq(
        SimpleInterpolation(dollar, Identifier(name)),
        StringPart(middle),
        SimpleInterpolation(dollar2, Identifier(pwd))), end)
  }

  "Parsing assignment" should "work" in {
    val s = "a = 42"
    val Seq(a, equals, fortyTwo, _) = MashLexer.tokenise(s).tokens
    parse(s) shouldEqual
      PatternAssignmentExpr(IdentPattern(a), equals, Literal(fortyTwo))
  }

  "Parsing 1 / def" should "not crash" in {
    val s = "1 / def"
    parse(s)
  }

  "Getting the text from tokens when parsing def" should "not crash" in {
    parse("def").tokens.map(_.text)
  }

  "Parsing dollar in mish" should "not crash" in {
    parse("!{$}")
  }

  "Parsing alias" should "not crash" in {
    parse("alias")
  }

  "Parsing ..." should "not crash" in {
    parse("...")
  }

  "Parsing {{{" should "not crash" in {
    parse("{{{")
  }

  "Parsing 'if ('" should "not crash" in {
    parse("if (")
  }

  "Parsing 'def directory namespace'" should "not crash" in {
    parse("def directory namespace")
  }

  "Parsing '(=> 1 +'" should "have a sensible region" in {
    parse("(=> 1 +").regionOpt shouldEqual Some(Region(0, 7))
  }

}