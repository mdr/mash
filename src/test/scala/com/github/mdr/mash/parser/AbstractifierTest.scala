package com.github.mdr.mash.parser

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.parser.{ ConcreteSyntax ⇒ Concrete }
import com.github.mdr.mash.runtime.MashString

class AbstractifierTest extends FlatSpec with Matchers {

  "Abstract representation of a string literal" should "not include the quotes" in {
    val s = "\"foo\""
    val Some(concreteExpr) = MashParser.parseExpr(s)
    val abstractExpr = Abstractifier.abstractify(concreteExpr)
    abstractExpr should equal(StringLiteral("foo", QuotationType.Double, tildePrefix = false, sourceInfoOpt = Some(SourceInfo(concreteExpr))))
  }

  "String literals that lack quotes" should "still work" in {
    val s = "\"foo"
    val Some(concreteExpr) = MashParser.parseExpr(s)
    val abstractExpr = Abstractifier.abstractify(concreteExpr)
    abstractExpr should equal(StringLiteral("foo", QuotationType.Double, tildePrefix = false, sourceInfoOpt = Some(SourceInfo(concreteExpr))))
  }

  "A function invocation" should "have its positional arguments collected" in {
    val s = "ls pwd"
    val Some(concreteExpr) = MashParser.parseExpr(s)
    val Concrete.InvocationExpr(function, Seq(arg: Concrete.Expr)) = concreteExpr

    val abstractExpr = Abstractifier.abstractify(concreteExpr)

    abstractExpr should equal(
      InvocationExpr(
        Identifier("ls", Some(SourceInfo(function))),
        Seq(Argument.PositionArg(Identifier("pwd", Some(SourceInfo(arg))), Some(SourceInfo(arg)))),
        sourceInfoOpt = Some(SourceInfo(concreteExpr))))
  }

  "A function invocation" should "have its flag arguments collected" in {
    val s = "foo --bar=baz"
    val Some(concreteExpr) = MashParser.parseExpr(s)
    val Concrete.InvocationExpr(function, Seq(longArg)) = concreteExpr
    val Concrete.LongArg(flagName, Some((equals, arg))) = longArg

    val abstractExpr = Abstractifier.abstractify(concreteExpr)

    abstractExpr should equal(
      InvocationExpr(
        Identifier("foo", Some(SourceInfo(function))),
        Seq(Argument.LongFlag("bar", Some(Identifier("baz", Some(SourceInfo(arg)))), Some(SourceInfo(longArg)))),
        sourceInfoOpt = Some(SourceInfo(concreteExpr))))
  }

}