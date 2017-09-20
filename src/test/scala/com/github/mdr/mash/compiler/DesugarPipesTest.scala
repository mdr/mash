package com.github.mdr.mash.compiler

import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.{ Abstractifier, MashParser, Provenance }
import org.scalatest.{ FlatSpec, Matchers }

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

  private def parse(s: String): Program = {
    val concreteProgram = MashParser.parseForgiving(s)
    val abstractProgram = new Abstractifier(Provenance.internal(s)).abstractify(concreteProgram)
    ParenRemover.removeParens(abstractProgram)
  }

  private def removeSourceInfo(program: Program): Program =
    program.transform { case e ⇒ e.withSourceInfoOpt(None) }.asInstanceOf[Program]

}