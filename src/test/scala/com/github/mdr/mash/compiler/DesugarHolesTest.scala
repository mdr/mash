package com.github.mdr.mash.compiler

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.parser.Abstractifier

class DesugarHolesTest extends FlatSpec with Matchers {

  "_" desugarsTo ("x => x")
  "foo _" desugarsTo ("x => foo x")
  "_ 42" desugarsTo ("x => x 42")
  "a | map (_.b)" desugarsTo ("a | map (x => x.b)")
  "ls | keepIf (_.isFile and _.size < 1000)" desugarsTo "ls | keepIf (x => x.isFile and x.size < 1000)"
  "y => _.size" desugarsTo "y => x => x.size"
  "_.foo | _.bar" desugarsTo "x => x.foo | x => x.bar"
  "foo --bar=_" desugarsTo "x => foo --bar=x"
  "_.foo (_.bar)" desugarsTo "x => x.foo (x => x.bar)"

  private implicit class RichString(s: String) {
    def desugarsTo(s2: String) {
      "DesugarHoles" should s"desugar holes in '$s'" in {
        val actualExpr = renameHoleVariable(removeSourceInfo(DesugarHoles.desugarHoles(parse(s))))
        val expectedExpr = removeSourceInfo(parse(s2))
        actualExpr should equal(expectedExpr)
      }
    }
  }

  private def parse(s: String): Expr = Abstractifier.abstractify(MashParser.parseExpr(s).get)

  private def removeSourceInfo(e: Expr): Expr = e.transform { case e ⇒ e.withSourceInfoOpt(None) }

  private def renameHoleVariable(e: Expr) = e.transform {
    case Identifier(DesugarHoles.VariableName, _)       ⇒ Identifier("x", None)
    case LambdaExpr(DesugarHoles.VariableName, body, _) ⇒ LambdaExpr("x", body, None)
  }

}