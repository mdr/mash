package com.github.mdr.mash.compiler

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.MashParser
import com.github.mdr.mash.parser.Abstractifier
import com.github.mdr.mash.parser.Provenance

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

  private implicit class RichString(source: String) {

    def desugarsTo(actualString: String) {
      "DesugarHoles" should s"desugar holes in '$source'" in {
        val actualExpr = renameHoleVariable(removeSourceInfo(DesugarHoles.desugarHoles(parse(source))))
        val expectedExpr = removeSourceInfo(parse(actualString))
        actualExpr should equal(expectedExpr)
      }
    }

  }

  private def parse(s: String): Expr = new Abstractifier(Provenance(s, "test")).abstractify(MashParser.parseForgiving(s))

  private def removeSourceInfo(e: Expr): Expr = e.transform { case e ⇒ e.withSourceInfoOpt(None) }

  private def renameHoleVariable(e: Expr): Expr = e.transform {
    case Identifier(DesugarHoles.VariableName, _) ⇒
      Identifier("x", None)
    case LambdaExpr(ParamList(Seq(FunctionParam(DesugarHoles.VariableName, isVariadic, defaultExprOpt, isLazy, _))), body, _) ⇒
      LambdaExpr(ParamList(Seq(FunctionParam("x", isVariadic, defaultExprOpt, isLazy, None))), body, None)
  }

}