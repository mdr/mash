package com.github.mdr.mash.compiler

import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.{ Abstractifier, MashParser, Provenance }
import org.scalatest.{ FlatSpec, Matchers }

class DesugarHolesTest extends FlatSpec with Matchers {

  "_" desugarsTo "x => x"
  "foo _" desugarsTo "x => foo x"
  "_ 42" desugarsTo "x => x 42"
  "a | map (_.b)" desugarsTo "a | map (x => x.b)"
  "ls | keepIf (_.isFile and _.size < 1000)" desugarsTo "ls | keepIf (x => x.isFile and x.size < 1000)"
  "y => _.size" desugarsTo "y => x => x.size"
  "_.foo | _.bar" desugarsTo "x => x.foo | x => x.bar"
  "foo --bar=_" desugarsTo "x => foo --bar=x"
  "_.foo (_.bar)" desugarsTo "x => x.foo (x => x.bar)"

  private implicit class RichString(source: String) {

    def desugarsTo(actualString: String) {
      "DesugarHoles" should s"desugar holes in '$source'" in {
        val actualExpr = renameHoleVariable(removeSourceInfo(DesugarHoles.desugarHoles(parse(source)).body))
        val expectedExpr = removeSourceInfo(parse(actualString).body)
        actualExpr should equal(expectedExpr)
      }
    }

  }

  private def parse(s: String): Program = {
    val abstractifier = new Abstractifier(Provenance(s, "test"))
    abstractifier.abstractify(MashParser.parseForgiving(s))
  }

  private def removeSourceInfo(expr: Expr): Expr = expr.transform { case e ⇒ e.withSourceInfoOpt(None) }

  private def renameHoleVariable(e: Expr): Expr = e.transform {
    case Identifier(DesugarHoles.VariableName, _) ⇒
      Identifier("x", None)
    case LambdaExpr(ParamList(Seq(FunctionParam(attributes, Some(DesugarHoles.VariableName), isVariadic, defaultExprOpt, patternOpt, _))), body, _) ⇒
      LambdaExpr(ParamList(Seq(FunctionParam(attributes, Some("x"), isVariadic, defaultExprOpt, patternOpt, None))), body, None)
  }

}