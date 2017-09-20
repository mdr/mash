package com.github.mdr.mash.compiler

import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.{ Abstractifier, MashParser, Provenance }
import org.scalatest.{ FlatSpec, Matchers }

class ParenRemoverTest extends FlatSpec with Matchers {

  "ParenRemover" should "remove parens" in {
    val s = "(foo)"
    val expr = parse(s)
    val ParenExpr(subExpr, _) = expr

    removeParens(expr) should equal(subExpr)
  }

  private def removeParens(expr: Expr): Expr = ParenRemover.removeParens(Program(None, expr)).body

  it should "remove nested parens" in {
    val s = "((foo))"
    val expr = parse(s)
    val ParenExpr(ParenExpr(subExpr, _), _) = expr
    removeParens(expr) should equal(subExpr)
  }

  "foo --bar=(baz)" afterParensRemovedShouldBe "foo --bar=baz"

  private implicit class RichString(s: String) {
    def afterParensRemovedShouldBe(s2: String) {
      "ParenRemover" should s"remove parens from '$s'" in {
        val actualExpr = removeSourceInfo(removeParens(parse(s)))
        val expectedExpr = removeSourceInfo(parse(s2))
        actualExpr should equal(expectedExpr)
      }
    }
  }

  private def parse(s: String): Expr = {
    val abstractifier = new Abstractifier(Provenance.internal(s))
    abstractifier.abstractify(MashParser.parseForgiving(s)).body
  }

  private def removeSourceInfo(expr: Expr) = expr.transform { case e ⇒ e.withSourceInfoOpt(None) }

}