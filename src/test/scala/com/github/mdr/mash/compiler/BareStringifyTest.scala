package com.github.mdr.mash.compiler

import com.github.mdr.mash.compiler.BareStringify.bareStringify
import com.github.mdr.mash.parser.AbstractSyntax.{ AstNode, Program }
import com.github.mdr.mash.parser.{ Abstractifier, MashParser, Provenance }
import org.scalatest.{ FlatSpec, Matchers }

class BareStringifyTest extends FlatSpec with Matchers {

  "unbound" ==> """ "unbound" """

  {
    implicit val bindings = Set("bound")
    "bound" ==> "bound"
  }

  "class A { def foo = fields }" ==> "class A { def foo = fields }"
  "class A { def foo = toString }" ==> "class A { def foo = toString }"
  "class A { def foo = bar; def bar = 42 }" ==> "class A { def foo = bar; def bar = 42 }"

  "def doSomething (@flag @(shortFlag d) dryRun = false) = 42" ==>
    """def doSomething (@flag @(shortFlag "d") dryRun = false) = 42"""

  private implicit class RichString(input: String)(implicit val bindings: Set[String] = Set()) {

    def ==>(expected: String): Unit = {
      s"Identifying rich strings in '$input'" should s"result in '$expected'" in {
        val inputProgram = compile(input)
        val actualProgram = removeSourceInfo(bareStringify(inputProgram, bindings))
        val expectedProgram = removeSourceInfo(compile(expected))
        actualProgram should equal(expectedProgram)
      }
    }

  }

  private def removeSourceInfo(expr: AstNode): AstNode = expr.transform { case e ⇒ e.withSourceInfoOpt(None) }

  private def compile(input: String): Program = {
    val concreteProgram = MashParser.parseForgiving(input)
    val abstractifier = new Abstractifier(Provenance(input, "test"))
    val abstractProgram = abstractifier.abstractify(concreteProgram)
    abstractProgram
  }
}
