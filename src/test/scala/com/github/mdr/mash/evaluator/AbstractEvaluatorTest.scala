package com.github.mdr.mash.evaluator

import com.github.mdr.mash.compiler.{ CompilationSettings, CompilationUnit, Compiler }
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.parser.ParseError
import com.github.mdr.mash.runtime.MashValue
import org.scalatest.{ FlatSpec, Matchers }

import scala.language.postfixOps

abstract class AbstractEvaluatorTest extends FlatSpec with Matchers {

  case class Config(bareWords: Boolean = false)

  protected implicit class RichString(rawS: String)(implicit config: Config = Config()) {

    private val s = rawS.stripMargin
    
    private def compile(s: String, bindings: Map[String, MashValue]): Expr = {
      val settings = CompilationSettings(bareWords = config.bareWords)
      Compiler.compile(CompilationUnit(s), bindings = bindings, settings) match {
        case Left(ParseError(message, _)) ⇒ throw new AssertionError("Compilation failed: " + message)
        case Right(expr)                  ⇒ expr
      }
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

    def shouldEvaluateTo(expected: Int): Unit = shouldEvaluateTo(expected.toString)

    def shouldEvaluateTo(expected: Boolean): Unit = shouldEvaluateTo(expected.toString)

    def shouldEvaluateTo(expected: Double): Unit = shouldEvaluateTo(expected.toString)

    def shouldEvaluateTo(expected: Null): Unit = shouldEvaluateTo("null")

    def shouldEvaluateTo(expectedString: String): Unit =
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