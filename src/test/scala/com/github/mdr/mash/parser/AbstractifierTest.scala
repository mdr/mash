package com.github.mdr.mash.parser

import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.{ ConcreteSyntax => Concrete }
import org.scalatest.{ FlatSpec, Matchers }

class AbstractifierTest extends FlatSpec with Matchers {

  "Abstract representation of a string literal" should "not include the quotes" in {
    val s = "\"foo\""
    val concreteExpr = parse(s)
    val provenance = Provenance(s, source = "test")

    val abstractExpr = new Abstractifier(provenance).abstractify(concreteExpr)

    abstractExpr should equal(
      StringLiteral("foo", QuotationType.Double, hasTildePrefix = false, sourceInfoOpt = Some(SourceInfo(provenance, concreteExpr))))
  }

  "String literals that lack quotes" should "still work" in {
    val s = "\"foo"
    val concreteExpr = parse(s)
    val provenance = Provenance(s, source = "test")

    val abstractExpr = new Abstractifier(provenance).abstractify(concreteExpr)

    abstractExpr should equal(
      StringLiteral("foo", QuotationType.Double, hasTildePrefix = false, sourceInfoOpt = Some(SourceInfo(provenance, concreteExpr))))
  }

  "A function invocation" should "have its positional arguments collected" in {
    val s = "ls pwd"
    val concreteExpr = parse(s)
    val Concrete.InvocationExpr(function, Seq(arg: Concrete.Expr)) = concreteExpr
    val provenance = Provenance(s, source = "test")

    val abstractExpr = new Abstractifier(provenance).abstractify(concreteExpr)

    abstractExpr should equal(
      InvocationExpr(
        Identifier("ls", Some(SourceInfo(provenance, function))),
        Seq(Argument.PositionArg(Identifier("pwd", Some(SourceInfo(provenance, arg))), Some(SourceInfo(provenance, arg)))),
        isParenInvocation = false,
        sourceInfoOpt = Some(SourceInfo(provenance, concreteExpr))))
  }

  "A function invocation" should "have its flag arguments collected" in {
    val s = "foo --bar=baz"
    val concreteExpr = parse(s)
    val Concrete.InvocationExpr(function, Seq(longArg)) = concreteExpr
    val Concrete.LongArg(flagName, Some((equals, arg))) = longArg
    val provenance = Provenance(s, source = "test")

    val abstractExpr = new Abstractifier(provenance).abstractify(concreteExpr)

    abstractExpr should equal(
      InvocationExpr(
        Identifier("foo", Some(SourceInfo(provenance, function))),
        Seq(Argument.LongFlag("bar", Some(Identifier("baz", Some(SourceInfo(provenance, arg)))), Some(SourceInfo(provenance, longArg)))),
        isParenInvocation = false,
        sourceInfoOpt = Some(SourceInfo(provenance, concreteExpr))))
  }

  private def parse(s: String) = MashParser.parseForgiving(s)

}