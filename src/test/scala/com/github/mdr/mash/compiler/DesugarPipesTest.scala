package com.github.mdr.mash.compiler
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.parser.Abstractifier

class DesugarPipesTest extends FlatSpec with Matchers {

  "a | b" desugarsTo "b a"
  "a | b c" desugarsTo "b c a"
  "a | b | c" desugarsTo "c (b a)"
  "a | b c | d e" desugarsTo "d e (b c a)"
  "foo --bar=(a | b)" desugarsTo "foo --bar=(b a)"

  private implicit class RichString(s: String) {
    def desugarsTo(s2: String) {
      "DesugarPipes" should s"desugar pipes in '$s'" in {
        val actualExpr = removeSourceInfo(DesugarPipes.desugarPipes(parse(s)))
        val expectedExpr = removeSourceInfo(parse(s2))
        actualExpr should equal(expectedExpr)
      }
    }

  }

  private def parse(s: String): Expr = ParenRemover.removeParens(Abstractifier.abstractify(MashParser.parse(s).get))

  private def removeSourceInfo(e: Expr) = e.transform { case e ⇒ e.withSourceInfoOpt(None) }

}