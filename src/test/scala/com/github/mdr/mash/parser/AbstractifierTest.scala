package com.github.mdr.mash.parser

import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.ConcreteSyntax.Program
import com.github.mdr.mash.parser.{ ConcreteSyntax ⇒ Concrete }
import org.scalatest.{ FlatSpec, Matchers }

class AbstractifierTest extends FlatSpec with Matchers {

  "Abstract representation of a string literal" should "not include the quotes" in {
    val s = "\"foo\""
    val concreteProgram = parse(s)
    val provenance = Provenance.internal(s)

    val abstractExpr = abstractify(concreteProgram, provenance)

    abstractExpr should equal(
      StringLiteral("foo", QuotationType.Double, hasTildePrefix = false, sourceInfoOpt = Some(SourceInfo(provenance, concreteProgram.body))))
  }

  "String literals that lack quotes" should "still work" in {
    val s = "\"foo"
    val concreteProgram = parse(s)
    val provenance = Provenance.internal(s)

    val abstractExpr = abstractify(concreteProgram, provenance)

    abstractExpr should equal(
      StringLiteral("foo", QuotationType.Double, hasTildePrefix = false, sourceInfoOpt = Some(SourceInfo(provenance, concreteProgram.body))))
  }

  "A function invocation" should "have its positional arguments collected" in {
    val s = "ls pwd"
    val concreteProgram = parse(s)
    val Concrete.InvocationExpr(function, Seq(arg: Concrete.Expr)) = concreteProgram.body
    val provenance = Provenance.internal(s)

    val abstractExpr = abstractify(concreteProgram, provenance)

    abstractExpr should equal(
      InvocationExpr(
        Identifier("ls", Some(SourceInfo(provenance, function))),
        Seq(Argument.PositionArg(Identifier("pwd", Some(SourceInfo(provenance, arg))), Some(SourceInfo(provenance, arg)))),
        isParenInvocation = false,
        sourceInfoOpt = Some(SourceInfo(provenance, concreteProgram.body))))
  }

  private def abstractify(program: Program, provenance: Provenance): Expr =
    new Abstractifier(provenance).abstractify(program).body

  "A function invocation" should "have its flag arguments collected" in {
    val s = "foo --bar=baz"
    val concreteProgram = parse(s)
    val Concrete.InvocationExpr(function, Seq(longArg)) = concreteProgram.body
    val Concrete.LongArg(_, Some((_, arg))) = longArg
    val provenance = Provenance.internal(s)

    val abstractExpr = abstractify(concreteProgram, provenance)

    abstractExpr should equal(
      InvocationExpr(
        Identifier("foo", Some(SourceInfo(provenance, function))),
        Seq(Argument.LongFlag("bar", Some(Identifier("baz", Some(SourceInfo(provenance, arg)))), Some(SourceInfo(provenance, longArg)))),
        isParenInvocation = false,
        sourceInfoOpt = Some(SourceInfo(provenance, concreteProgram.body))))
  }

  private def parse(s: String): Program = MashParser.parseForgiving(s)

}