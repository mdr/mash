package com.github.mdr.mash.compiler

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.parser.Abstractifier

class ParenRemoverTest extends FlatSpec with Matchers {

  "ParenRemover" should "remove parens" in {
    val s = "(foo)"
    val expr = parse(s)
    val ParenExpr(subExpr, _) = expr

    ParenRemover.removeParens(expr) should equal(subExpr)
  }

  it should "remove nested parens" in {
    val s = "((foo))"
    val expr = parse(s)
    val ParenExpr(ParenExpr(subExpr, _), _) = expr
    ParenRemover.removeParens(expr) should equal(subExpr)
  }

  "foo --bar=(baz)" afterParensRemovedShouldBe ("foo --bar=baz")

  private implicit class RichString(s: String) {
    def afterParensRemovedShouldBe(s2: String) {
      "ParenRemover" should s"remove parens from '$s'" in {
        val actualExpr = removeSourceInfo(ParenRemover.removeParens(parse(s)))
        val expectedExpr = removeSourceInfo(parse(s2))
        actualExpr should equal(expectedExpr)
      }
    }

  }

  private def parse(s: String): Expr = Abstractifier.abstractify(MashParser.parseExpr(s).get)

  private def removeSourceInfo(e: Expr) = e.transform { case e ⇒ e.withSourceInfoOpt(None) }

}

