package com.github.mdr.mash.completions

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.github.mdr.mash.compiler.Compiler
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.evaluator.StandardEnvironment
import com.github.mdr.mash.compiler.CompilationUnit

class InvocationFinderTest extends FlatSpec with Matchers {

  "Finding a positional literal argument" should "work" in {
    invocationPos("f 'arg'") should equal(0)
    invocationPos("f arg1 'arg2' arg3") should equal(1)
    invocationPos("f --arg1 'arg2' arg3") should equal(1)
  }

  "Finding a paren literal argument" should "work" in {
    invocationPos("f('arg')") should equal(0)
    invocationPos("f(arg1, 'arg2', arg3)") should equal(1)
  }

  "Finding a flag literal argument" should "work" in {
    invocationPos("f --arg='arg'") should equal(0)
    invocationPos("f arg1 --arg2='arg2'") should equal(1)
  }

  "Finding a pipe literal argument" should "work" in {
    invocationPos("'arg' | f") should equal(0)
    invocationPos("'arg2' | f arg1") should equal(1)
    invocationPos("arg2 | f --arg1='arg1'") should equal(0)
  }

  private def invocationPos(s: String): Int = {
    val Some(expr) = Compiler.compile(CompilationUnit(s), StandardEnvironment.create.bindings, forgiving = true)
    val Some(literalToken) = expr.sourceInfoOpt.get.expr.tokens.find(_.isString)
    val Some(InvocationInfo(_, pos)) = InvocationFinder.findInvocationWithLiteralArg(expr, literalToken)
    pos
  }

}