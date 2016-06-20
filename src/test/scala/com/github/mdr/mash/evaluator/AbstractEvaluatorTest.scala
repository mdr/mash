package com.github.mdr.mash.evaluator

import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.compiler.Compiler
import org.scalatest.junit.JUnitRunner
import com.github.mdr.mash.parser.MashParserException
import scala.language.postfixOps

abstract class AbstractEvaluatorTest extends FlatSpec with Matchers {

  protected implicit class RichString(s: String) {

    def shouldThrowAnException =
      "Evaluator" should s"throw an exception when evaluating '$s'" in {
        val env = StandardEnvironment.create
        val Some(expr) = Compiler.compile(s, forgiving = false, bindings = env.valuesMap)
        try {
          val result = Evaluator.evaluate(expr)(EvaluationContext(ScopeStack(env.globalVariables), expr))
          fail("Expected an exception during evaluation, but got a result of: " + result)
        } catch {
          case _: EvaluatorException ⇒ // exception expected here
        }
      }

    def shouldNotThrowAnException =
      "Evaluator" should s"not throw an exception when evaluating '$s'" in {
        val env = StandardEnvironment.create
        val Some(expr) = Compiler.compile(s, forgiving = false, bindings = env.valuesMap)
        Evaluator.evaluate(expr)(EvaluationContext(ScopeStack(env.globalVariables), expr))
      }

    def shouldEvaluateTo(expectedString: String) =
      "Evaluator" should s"evaluate '$s' to '$expectedString'" in {
        val env = StandardEnvironment.create

        val Some(expr1) = Compiler.compile(s, forgiving = false, bindings = env.bindings)
        val ctx1 = EvaluationContext(ScopeStack(env.globalVariables), expr1)
        val actual = Evaluator.evaluate(expr1)(ctx1)

        val Some(expr2) = Compiler.compile(expectedString, forgiving = false, bindings = env.bindings)
        val ctx2 = EvaluationContext(ScopeStack(env.globalVariables), expr2)
        val expected = Evaluator.evaluate(expr2)(ctx2)

        actual should equal(expected)
      }

  }

}