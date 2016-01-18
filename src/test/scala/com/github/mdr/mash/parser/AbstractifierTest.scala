package com.github.mdr.mash.parser

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.evaluator.MashNumber
import com.github.mdr.mash.parser.{ ConcreteSyntax ⇒ Concrete }
import com.github.mdr.mash.evaluator.MashString

class AbstractifierTest extends FlatSpec with Matchers {

  "Abstract representation of a string literal" should "not include the quotes" in {
    val s = "\"foo\""
    val Some(concreteExpr) = MashParser.parse(s)
    val abstractExpr = Abstractifier.abstractify(concreteExpr)
    abstractExpr should equal(StringLiteral("foo", QuotationType.Double, tildePrefix = false, sourceInfoOpt = Some(SourceInfo(concreteExpr))))
  }

  "String literals that lack quotes" should "still work" in {
    val s = "\"foo"
    val Some(concreteExpr) = MashParser.parse(s)
    val abstractExpr = Abstractifier.abstractify(concreteExpr)
    abstractExpr should equal(StringLiteral("foo", QuotationType.Double, tildePrefix = false, sourceInfoOpt = Some(SourceInfo(concreteExpr))))
  }

  "A chained comparison" should "get desugared into a conjunction of binary comparisons" in {

    val s = "0 <= x < 100"
    val Some(concreteExpr) = MashParser.parse(s)
    val Concrete.ChainedOpExpr(zeroLiteral, Seq((lt1, xIdentifier), (lt2, hundredLiteral))) = concreteExpr
    val abstractExpr = Abstractifier.abstractify(concreteExpr)

    abstractExpr should equal(
      BinOpExpr(
        BinOpExpr(
          Literal(MashNumber(0), Some(SourceInfo(zeroLiteral))),
          BinaryOperator.LessThanEquals,
          Identifier("x", Some(SourceInfo(xIdentifier))),
          Some(SourceInfo(concreteExpr))),
        BinaryOperator.And,
        BinOpExpr(
          Identifier("x", Some(SourceInfo(xIdentifier))),
          BinaryOperator.LessThan,
          Literal(MashNumber(100), Some(SourceInfo(hundredLiteral))),
          Some(SourceInfo(concreteExpr))),
        Some(SourceInfo(concreteExpr))))

  }

  "A function invocation" should "have its positional arguments collected" in {
    val s = "ls pwd"
    val Some(concreteExpr) = MashParser.parse(s)
    val Concrete.InvocationExpr(function, Seq(arg: Concrete.Expr)) = concreteExpr

    val abstractExpr = Abstractifier.abstractify(concreteExpr)

    abstractExpr should equal(
      InvocationExpr(
        Identifier("ls", Some(SourceInfo(function))),
        Seq(Argument.PositionArg(Identifier("pwd", Some(SourceInfo(arg))))),
        sourceInfoOpt = Some(SourceInfo(concreteExpr))))
  }

  "A function invocation" should "have its flag arguments collected" in {
    val s = "foo --bar=baz"
    val Some(concreteExpr) = MashParser.parse(s)
    val Concrete.InvocationExpr(function, Seq(longArg)) = concreteExpr
    val Concrete.LongArg(flagName, Some((equals, arg))) = longArg

    val abstractExpr = Abstractifier.abstractify(concreteExpr)

    abstractExpr should equal(
      InvocationExpr(
        Identifier("foo", Some(SourceInfo(function))),
        Seq(Argument.LongFlag("bar", Some(Identifier("baz", Some(SourceInfo(arg)))))),
        sourceInfoOpt = Some(SourceInfo(concreteExpr))))
  }

}