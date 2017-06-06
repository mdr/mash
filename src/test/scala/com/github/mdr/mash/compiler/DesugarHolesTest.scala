package com.github.mdr.mash.compiler

import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.parser.{ Abstractifier, MashParser, Provenance }
import org.scalatest.{ FlatSpec, Matchers }

class DesugarHolesTest extends FlatSpec with Matchers {

  "_" desugarsTo "x1 => x1"
  "foo _" desugarsTo "x1 => foo x1"
  "_ 42" desugarsTo "x1 => x1 42"
  "a | map (_.b)" desugarsTo "a | map (x1 => x1.b)"
  "ls | keepIf (_.isFile and _.size < 1000)" desugarsTo "ls | keepIf (x1 => x1.isFile and x1.size < 1000)"
  "y => _.size" desugarsTo "y => x1 => x1.size"
  "_.foo | _.bar" desugarsTo "x1 => x1.foo | x1 => x1.bar"
  "foo --bar=_" desugarsTo "x1 => foo --bar=x1"
  "_.foo (_.bar)" desugarsTo "x1 => x1.foo (x1 => x1.bar)"

  "_1 * _2" desugarsTo "x1 x2 => x1 * x2"
  "_2" desugarsTo "x1 x2 => x2"

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


  object HoleVar {

    def unapply(s: String): Option[Int] =
      if (s startsWith DesugarHoles.VariableNamePrefix)
        Some(s.drop(DesugarHoles.VariableNamePrefix.length).toInt)
      else
        None

  }

  private def renameHoleVariable(e: Expr): Expr = e.transform {
    case Identifier(HoleVar(n), _)                                                                                                                                               ⇒
      val newName = "x" + n
      Identifier(newName, None)
    case FunctionParam(attributes, Some(HoleVar(n)), isVariadic, defaultExprOpt, _, source) ⇒
      val newName = "x" + n
      FunctionParam(attributes, Some(newName), isVariadic, defaultExprOpt, Some(IdentPattern(newName)), None)
  }

}