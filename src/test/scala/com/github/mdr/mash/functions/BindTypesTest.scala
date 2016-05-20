package com.github.mdr.mash.functions

import org.scalatest._

import com.github.mdr.mash.compiler.Compiler
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.inference.SimpleTypedArguments
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.parser.AbstractSyntax.InvocationExpr
import com.github.mdr.mash.parser.MashParserException

class BindTypesTest extends FlatSpec with Matchers {

  "A single positional argument" should "be tied to its parameter" in {
    val param = Parameter("param", "test param")
    ParameterModel(Seq(param)).boundTypeParams("f 1").paramAt(0) should equal(Some(param))
  }

  "Multiple positional arguments" should "be tied to ordinary positional parameters" in {
    val param1 = Parameter("param1", "test param")
    val param2 = Parameter("param2", "test param")
    val param3 = Parameter("param3", "test param")
    ParameterModel(Seq(param1, param2, param3)).boundTypeParams("f 1 2 3").posToParam should equal(Map(
      0 -> param1, 1 -> param2, 2 -> param3))
  }

  "Multiple positional arguments" should "be tied to a single variadic parameter" in {
    val param = Parameter("param", "test param", isVariadic = true)
    ParameterModel(Seq(param)).boundTypeParams("f 1 2 3").posToParam should equal(Map(
      0 -> param, 1 -> param, 2 -> param))
  }

  "Extra positional arguments" should "be tied to a single variadic parameter" in {
    val param1 = Parameter("param1", "test param")
    val param2 = Parameter("param2", "test param", isVariadic = true)
    ParameterModel(Seq(param1, param2)).boundTypeParams("f 1 2 3").posToParam should equal(Map(
      0 -> param1, 1 -> param2, 2 -> param2))
  }

  "Named arguments" should "be tied to their parameter" in {
    val param = Parameter("param", "test param")
    ParameterModel(Seq(param)).boundTypeParams("f --param=1").posToParam should equal(Map(0 -> param))
  }

  "Multiple named arguments" should "be tied to their parameter" in {
    val param1 = Parameter("param1", "test param")
    val param2 = Parameter("param2", "test param")
    ParameterModel(Seq(param1, param2)).boundTypeParams("f --param2=1 --param1=2").posToParam should equal(
      Map(0 -> param2, 1 -> param1))
  }

  "Last arguments" should "be tied to the last parameter with variadics" in {
    val param1 = Parameter("param1", "test param", isVariadic = true)
    val param2 = Parameter("param2", "test param", isLast = true)
    ParameterModel(Seq(param1, param2)).boundTypeParams("f 1 2 3").posToParam should equal(
      Map(0 -> param1, 1 -> param1, 2 -> param2))
  }

  "Last arguments" should "be tied to the last parameter with optionals" in {
    val param1 = Parameter("param1", "test param", defaultValueGeneratorOpt = Some(() ⇒ true))
    val param2 = Parameter("param2", "test param", isLast = true)
    ParameterModel(Seq(param1, param2)).boundTypeParams("f 1").posToParam should equal(
      Map(0 -> param2))
  }

  "Last arguments" should "not be used if specified by name" in {
    val param1 = Parameter("param1", "test param", defaultValueGeneratorOpt = Some(() ⇒ true))
    val param2 = Parameter("param2", "test param", isLast = true)
    ParameterModel(Seq(param1, param2)).boundTypeParams("f 1 --param2=2").posToParam should equal(
      Map(0 -> param1, 1 -> param2))
  }

  "All the features" should "work together" in {
    val optionalParam = Parameter("optional", "test param", defaultValueGeneratorOpt = Some(() ⇒ true))
    val variadicParam = Parameter("variadic", "test param", isVariadic = true)
    val lastParam = Parameter("last", "test param", isLast = true)
    val positionalParam = Parameter("positional", "test param")
    val namedParam = Parameter("named", "test param", isFlag = true)
    val parameters = ParameterModel(Seq(positionalParam, optionalParam, variadicParam, lastParam, namedParam))
    parameters.boundTypeParams("f 1 2 3 4 5 --named=5").posToParam should equal(
      Map(0 -> positionalParam, 1 -> optionalParam, 2 -> variadicParam, 3 -> variadicParam, 4 -> lastParam, 5 -> namedParam))
  }

  implicit class RichParameterModel(parameters: ParameterModel) {
    def boundTypeParams(s: String): BoundTypeParams =
      parameters.bindTypes(getArguments(s))

    private def getArguments(s: String): TypedArguments = {
      val Some(expr) = Compiler.compile(s, Environment.create)
      val Some(invocationExpr) = expr.find {
        case iexpr: InvocationExpr ⇒ iexpr
      }
      SimpleTypedArguments.from(invocationExpr)
    }
  }

}