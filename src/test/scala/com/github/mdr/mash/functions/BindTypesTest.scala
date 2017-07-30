package com.github.mdr.mash.functions

import com.github.mdr.mash.compiler.{ CompilationUnit, Compiler }
import com.github.mdr.mash.evaluator.StandardEnvironment
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.parser.AbstractSyntax.InvocationExpr
import org.scalatest.{ FlatSpec, Matchers }

class BindTypesTest extends FlatSpec with Matchers {

  "A single positional argument" should "be tied to its parameter" in {
    val param = Parameter(Some("param"), Some("test param"))
    ParameterModel(Seq(param)).boundTypeParams("f 1").paramAt(0) should equal(Some(param))
  }

  "Multiple positional arguments" should "be tied to ordinary positional parameters" in {
    val param1 = Parameter(Some("param1"), Some("test param"))
    val param2 = Parameter(Some("param2"), Some("test param"))
    val param3 = Parameter(Some("param3"), Some("test param"))
    ParameterModel(Seq(param1, param2, param3)).boundTypeParams("f 1 2 3").posToParam should equal(Map(
      0 -> param1, 1 -> param2, 2 -> param3))
  }

  "Multiple positional arguments" should "be tied to a single variadic parameter" in {
    val param = Parameter(Some("param"), Some("test param"), isVariadic = true)
    ParameterModel(Seq(param)).boundTypeParams("f 1 2 3").posToParam should equal(Map(
      0 -> param, 1 -> param, 2 -> param))
  }

  "Extra positional arguments" should "be tied to a single variadic parameter" in {
    val param1 = Parameter(Some("param1"), Some("test param"))
    val param2 = Parameter(Some("param2"), Some("test param"), isVariadic = true)
    ParameterModel(Seq(param1, param2)).boundTypeParams("f 1 2 3").posToParam should equal(Map(
      0 -> param1, 1 -> param2, 2 -> param2))
  }

  "Named arguments" should "be tied to their parameter" in {
    val param = Parameter(Some("param"), Some("test param"))
    ParameterModel(Seq(param)).boundTypeParams("f --param=1").posToParam should equal(Map(0 -> param))
  }

  "Multiple named arguments" should "be tied to their parameter" in {
    val param1 = Parameter(Some("param1"), Some("test param"))
    val param2 = Parameter(Some("param2"), Some("test param"))
    ParameterModel(Seq(param1, param2)).boundTypeParams("f --param2=1 --param1=2").posToParam should equal(
      Map(0 -> param2, 1 -> param1))
  }

  "Last arguments" should "be tied to the last parameter with variadics" in {
    val param1 = Parameter(Some("param1"), Some("test param"), isVariadic = true)
    val param2 = Parameter(Some("param2"), Some("test param"))
    ParameterModel(Seq(param1, param2)).boundTypeParams("f 1 2 3").posToParam should equal(
      Map(0 -> param1, 1 -> param1, 2 -> param2))
  }

  "Last arguments" should "be tied to the last parameter with optionals" in {
    val param1 = Parameter(Some("param1"), Some("test param"), defaultValueGeneratorOpt = Some(true))
    val param2 = Parameter(Some("param2"), Some("test param"))
    ParameterModel(Seq(param1, param2)).boundTypeParams("f 1").posToParam should equal(
      Map(0 -> param2))
  }

  "Last arguments" should "not be used if specified by name" in {
    val param1 = Parameter(Some("param1"), Some("test param"), defaultValueGeneratorOpt = Some(true))
    val param2 = Parameter(Some("param2"), Some("test param"))
    ParameterModel(Seq(param1, param2)).boundTypeParams("f 1 --param2=2").posToParam should equal(
      Map(0 -> param1, 1 -> param2))
  }

  "All the features" should "work together" in {
    val optionalParam = Parameter(Some("optional"), Some("test param"), defaultValueGeneratorOpt = Some(true))
    val variadicParam = Parameter(Some("variadic"), Some("test param"), isVariadic = true)
    val lastParam = Parameter(Some("last"), Some("test param"))
    val positionalParam = Parameter(Some("positional"), Some("test param"))
    val namedParam = Parameter(Some("named"), Some("test param"), isFlag = true)
    val parameters = ParameterModel(Seq(positionalParam, optionalParam, variadicParam, lastParam, namedParam))
    parameters.boundTypeParams("f 1 2 3 4 5 --named=5").posToParam should equal(
      Map(0 -> positionalParam, 1 -> optionalParam, 2 -> variadicParam, 3 -> variadicParam, 4 -> lastParam, 5 -> namedParam))
  }

  implicit class RichParameterModel(parameters: ParameterModel) {
    def boundTypeParams(s: String): BoundTypeParams =
      parameters.bindTypes(getArguments(s))

    private def getArguments(s: String): TypedArguments = {
      val expr = Compiler.compileForgiving(CompilationUnit(s), StandardEnvironment.create.bindings)
      val Some(invocationExpr) = expr.find { case iexpr: InvocationExpr ⇒ iexpr }
      TypedArguments.from(invocationExpr)
    }
  }

}