package com.github.mdr.mash.compiler
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.parser.Abstractifier
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import com.github.mdr.mash.parser.Provenance

@RunWith(classOf[JUnitRunner])
class DesugarPipesTest extends FlatSpec with Matchers {

  "a | b" desugarsTo "b a"
  "a | b c" desugarsTo "b c a"
  "a | b | c" desugarsTo "c (b a)"
  "a | b c | d e" desugarsTo "d e (b c a)"
  "foo --bar=(a | b)" desugarsTo "foo --bar=(b a)"

  private implicit class RichString(s: String) {
    def desugarsTo(expected: String) {
      "DesugarPipes" should s"desugar pipes in '$s'" in {
        val actualExpr = removeSourceInfo(DesugarPipes.desugarPipes(parse(s)))
        val expectedExpr = removeSourceInfo(parse(expected))
        actualExpr.sourceInfoOpt should equal(expectedExpr.sourceInfoOpt)
        actualExpr should equal(expectedExpr)
      }
    }
  }

  private def parse(s: String): Expr = {
    val concreteExpr = MashParser.parseForgiving(s)
    val abstractExpr = new Abstractifier(Provenance(s, "test")).abstractify(concreteExpr)
    ParenRemover.removeParens(abstractExpr)
  }

  private def removeSourceInfo(e: Expr) = e.transform { case e ⇒ e.withSourceInfoOpt(None) }

}