package com.github.mdr.mash.evaluator

import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.compiler.Compiler
import org.scalatest.junit.JUnitRunner
import com.github.mdr.mash.parser.MashParserException
import scala.language.postfixOps
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.compiler.CompilationSettings
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.runtime.MashValue

abstract class AbstractEvaluatorTest extends FlatSpec with Matchers {

  case class Config(bareWords: Boolean = false)

  protected implicit class RichString(s: String)(implicit config: Config = Config()) {

    private def compile(s: String, bindings: Map[String, MashValue]): Expr = {
      val settings = CompilationSettings(forgiving = false, bareWords = config.bareWords)
      Compiler.compile(CompilationUnit(s), bindings = bindings, settings)
    }

    def shouldThrowAnException =
      "Evaluator" should s"throw an exception when evaluating '$s'" in {
        val env = StandardEnvironment.create
        val expr = compile(s, env.valuesMap)
        try {
          val result = Evaluator.evaluate(expr)(EvaluationContext(ScopeStack(env.globalVariables.fields)))
          fail("Expected an exception during evaluation, but got a result of: " + result)
        } catch {
          case _: EvaluatorException ⇒ // exception expected here
        }
      }

    def shouldNotThrowAnException =
      "Evaluator" should s"not throw an exception when evaluating '$s'" in {
        val env = StandardEnvironment.create
        val expr = compile(s, env.valuesMap)
        Evaluator.evaluate(expr)(EvaluationContext(ScopeStack(env.globalVariables.fields)))
      }

    def shouldEvaluateTo(expectedString: String) =
      "Evaluator" should s"evaluate '$s' to '$expectedString'" in {
        val env = StandardEnvironment.create

        val expr1 = compile(s, env.valuesMap)
        val ctx1 = EvaluationContext(ScopeStack(env.globalVariables.fields))
        val actual = Evaluator.evaluate(expr1)(ctx1)

        val expr2 = compile(expectedString, env.valuesMap)
        val ctx2 = EvaluationContext(ScopeStack(env.globalVariables.fields))
        val expected = Evaluator.evaluate(expr2)(ctx2)

        actual should equal(expected)
      }

  }

}