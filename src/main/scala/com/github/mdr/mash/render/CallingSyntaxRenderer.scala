package com.github.mdr.mash.render

import com.github.mdr.mash.classes.BoundMethod
import com.github.mdr.mash.compiler.DesugarHoles
import com.github.mdr.mash.functions.{ MashFunction, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.lexer.TokenType.{ IDENTIFIER, LONG_FLAG, SHORT_FLAG }
import com.github.mdr.mash.render.MashRenderer.getTokenStyle
import com.github.mdr.mash.screen.Style.StylableString
import com.github.mdr.mash.screen.StyledString

object CallingSyntaxRenderer {

  def render(f: MashFunction): StyledString =
    f.name.style(getTokenStyle(IDENTIFIER)) + " ".style + render(f.params)

  def render(bm: BoundMethod): StyledString = render(bm.method)

  def render(method: MashMethod): StyledString =
    method.exampleTargetName.style(getTokenStyle(IDENTIFIER)) + ".".style + method.name.style(getTokenStyle(IDENTIFIER)) +
      " ".style + render(method.params)

  def render(params: ParameterModel): StyledString = {
    val positionalParams = params.params.filterNot(_.isFlag).map(renderPositionalParam)
    val flagParams = params.params.filter(_.isFlag).map(renderFlagParam)
    " ".style.join(flagParams ++ positionalParams)
  }

  private def renderFlagParam(param: Parameter): StyledString = {
    val name = param.nameOpt getOrElse Parameter.AnonymousParamName
    val flagValueName = param.flagValueNameOpt.getOrElse("value")
    val flagValueSuffix =
      if (param.isBooleanFlag)
        "".style
      else if (param.isFlagValueMandatory)
        "=<".style + flagValueName.style(getTokenStyle(IDENTIFIER)) + ">".style
      else
        "[=<".style + flagValueName.style(getTokenStyle(IDENTIFIER)) + ">]".style
    val longForm = s"--$name".style(getTokenStyle(LONG_FLAG)) + flagValueSuffix
    val main = param.shortFlagOpt match {
      case Some(shortFlag) ⇒ longForm + " | ".style + s"-$shortFlag".style(getTokenStyle(SHORT_FLAG))
      case None            ⇒ longForm
    }
    if (param.shortFlagOpt.isDefined)
      "(".style + main + ")".style
    else
      main
  }

  private def renderPositionalParam(param: Parameter): StyledString = {
    val paramName = param.nameOpt
      .filterNot(_ startsWith DesugarHoles.VariableNamePrefix)
      .getOrElse(Parameter.AnonymousParamName)
    val renderedParam = "<".style + paramName.style(getTokenStyle(IDENTIFIER)) + ">".style
    if (param.isVariadic)
      if (param.variadicAtLeastOne)
        renderedParam + "+...".style
      else
        renderedParam + "...".style
    else if (param.hasDefault)
      "[".style + renderedParam + "]".style
    else
      renderedParam
  }
}
