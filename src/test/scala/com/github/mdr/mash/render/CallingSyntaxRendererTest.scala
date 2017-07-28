package com.github.mdr.mash.render

import com.github.mdr.mash.functions.{ Parameter, ParameterModel }
import com.github.mdr.mash.render.CallingSyntaxRenderer.render
import org.scalatest.{ FlatSpec, Matchers }

class CallingSyntaxRendererTest extends FlatSpec with Matchers {

  val Positional = Parameter(
    nameOpt = Some("positional"))
  val Optional = Parameter(
    nameOpt = Some("optional"),
    defaultValueGeneratorOpt = Some(false))
  val Variadic = Parameter(
    nameOpt = Some("variadic"),
    isVariadic = true)
  val VariadicAtLeastOne = Parameter(
    nameOpt = Some("variadicAtLeastOne"),
    isVariadic = true,
    variadicAtLeastOne = true)

  val Flag = Parameter(
    nameOpt = Some("flag"),
    isFlag = true)
  val FlagWithShortName = Parameter(
    nameOpt = Some("flagWithShortName"),
    isFlag = true,
    isBooleanFlag = true,
    shortFlagOpt = Some('f'))
  val FlagWithCustomValueName = Parameter(
    nameOpt = Some("flagWithCustomValueName"),
    isFlag = true,
    flagValueNameOpt = Some("name"))
  val BooleanFlag = Parameter(
    nameOpt = Some("booleanFlag"),
    isFlag = true,
    isBooleanFlag = true)
  val MandatoryValueFlag = Parameter(
    nameOpt = Some("mandatoryValueFlag"),
    isFlag = true,
    isFlagValueMandatory = true)


  "Rendering calling syntax" should "render positional parameters correctly" in {
    val parameterModel = ParameterModel(Positional, Optional, Variadic)

    render(parameterModel).forgetStyling shouldEqual "<positional> [<optional>] <variadic>..."
  }

  it should "correctly render variadic parameters with at least one argument required" in  {
    val parameterModel = ParameterModel(VariadicAtLeastOne)

    render(parameterModel).forgetStyling shouldEqual "<variadicAtLeastOne>+..."
  }

  it should "correctly render flag parameters" in {
    val parameterModel = ParameterModel(Flag, FlagWithShortName, FlagWithCustomValueName, MandatoryValueFlag, BooleanFlag)

    render(parameterModel).forgetStyling shouldEqual
      "--flag[=<value>] (--flagWithShortName | -f) --flagWithCustomValueName[=<name>] --mandatoryValueFlag=<value> --booleanFlag"
  }

}
