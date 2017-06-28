package com.github.mdr.mash.evaluator

import com.github.mdr.mash.compiler.{ CompilationSettings, CompilationUnit, Compiler }
import com.github.mdr.mash.parser.AbstractSyntax.Expr
import com.github.mdr.mash.parser.ParseError
import com.github.mdr.mash.runtime.MashValue
import org.scalatest.{ FlatSpec, Matchers }

abstract class AbstractEvaluatorTest extends FlatSpec with Matchers {

  case class Config(bareWords: Boolean = false)

  protected def havingFirstRun(s: String)(f: Environment ⇒ Any) = {
    implicit val environment = StandardEnvironment.create
    val expr = compile(s, environment.bindings, Config())
    implicit val context = EvaluationContext(ScopeStack(environment.globalVariables))
    Evaluator.evaluate(expr)
    f(environment)
  }

  private def compile(s: String, bindings: Map[String, MashValue], config: Config): Expr = {
    val settings = CompilationSettings(bareWords = config.bareWords)
    Compiler.compile(CompilationUnit(s), bindings = bindings, settings) match {
      case Left(ParseError(message, _)) ⇒ throw new AssertionError("Compilation failed: " + message)
      case Right(program)               ⇒ program.body
    }
  }

  protected implicit class RichString(rawS: String)(implicit config: Config = Config(), env: Environment = StandardEnvironment.create) {

    private val s = rawS.stripMargin

    def shouldThrowAnException =
      "Evaluator" should s"throw an exception when evaluating '$s'" in {
        val expr = compile(s, env.bindings, config)
        try {
          val result = Evaluator.evaluate(expr)(EvaluationContext(ScopeStack(env.globalVariables)))
          fail("Expected an exception during evaluation, but got a result of: " + result)
        } catch {
          case _: EvaluatorException ⇒ // exception expected here
        }
      }

    def shouldNotThrowAnException =
      "Evaluator" should s"not throw an exception when evaluating '$s'" in {
        val expr = compile(s, env.bindings, config)
        Evaluator.evaluate(expr)(EvaluationContext(ScopeStack(env.globalVariables)))
      }

    def ==>(expected: Int): Unit = ==>(expected.toString)

    def ==>(expected: Boolean): Unit = ==>(expected.toString)

    def ==>(expected: Double): Unit = ==>(expected.toString)

    def ==>(expected: Null): Unit = ==>("null")

    def ==>(expectedString: String): Unit =
      "Evaluator" should s"evaluate '$s' to '$expectedString'" in {

        val expr1 = compile(s, env.bindings, config)
        val ctx1 = EvaluationContext(ScopeStack(env.globalVariables))
        val actual = Evaluator.evaluate(expr1)(ctx1)

        val expr2 = compile(expectedString, env.bindings, config)
        val ctx2 = EvaluationContext(ScopeStack(env.globalVariables))
        val expected = Evaluator.evaluate(expr2)(ctx2)

        actual should equal(expected)
      }

  }

}