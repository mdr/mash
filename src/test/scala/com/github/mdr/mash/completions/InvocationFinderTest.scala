package com.github.mdr.mash.completions

import com.github.mdr.mash.compiler.{ CompilationUnit, Compiler }
import com.github.mdr.mash.evaluator.StandardEnvironment
import org.scalatest.{ FlatSpec, Matchers }

class InvocationFinderTest extends FlatSpec with Matchers {

  "Finding a positional literal argument" should "work" in {
    invocationPos("f 'arg'") shouldEqual 0
    invocationPos("f arg1 'arg2' arg3") shouldEqual 1
    invocationPos("f --arg1 'arg2' arg3") shouldEqual 1
  }

  "Finding a paren literal argument" should "work" in {
    invocationPos("f('arg')") shouldEqual 0
    invocationPos("f(arg1, 'arg2', arg3)") shouldEqual 1
  }

  "Finding a flag literal argument" should "work" in {
    invocationPos("f --arg='arg'") shouldEqual 0
    invocationPos("f arg1 --arg2='arg2'") shouldEqual 1
  }

  "Finding a pipe literal argument" should "work" in {
    invocationPos("'arg' | f") shouldEqual 0
    invocationPos("'arg2' | f arg1") shouldEqual 1
    invocationPos("arg2 | f --arg1='arg1'") shouldEqual 0
  }

  private def invocationPos(s: String): Int = {
    val expr = Compiler.compileForgiving(CompilationUnit(s), StandardEnvironment.create.bindings).body
    val Some(literalToken) = expr.sourceInfoOpt.get.node.tokens.find(_.isString)
    val Some(InvocationInfo(_, pos)) = InvocationFinder.findInvocationWithLiteralArg(expr, literalToken)
    pos
  }

}