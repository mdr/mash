package com.github.mdr.mash.render

import com.github.mdr.mash.classes.BoundMethod
import com.github.mdr.mash.compiler.DesugarHoles
import com.github.mdr.mash.functions.{ MashFunction, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.lexer.TokenType.{ IDENTIFIER, LONG_FLAG, SHORT_FLAG }
import com.github.mdr.mash.render.MashRenderer.getTokenStyle
import com.github.mdr.mash.screen.Style._
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
    val longForm = renderLongFlagForm(param)
    param.shortFlagOpt match {
      case Some(shortFlag) ⇒
        val shortForm = s"-$shortFlag".style(getTokenStyle(SHORT_FLAG))
        style"($longForm | $shortForm)"
      case None            ⇒
        longForm
    }
  }

  private def renderLongFlagForm(param: Parameter): StyledString = {
    val name = param.nameOpt getOrElse Parameter.AnonymousParamName
    val flagValueSuffix =
      if (param.isBooleanFlag)
        "".style
      else {
        val flagValueName = param.flagValueNameOpt.getOrElse("value").style(getTokenStyle(IDENTIFIER))
        if (param.isFlagValueMandatory)
          style"=<$flagValueName>"
        else
          style"[=<$flagValueName>]"
      }
    s"--$name".style(getTokenStyle(LONG_FLAG)) + flagValueSuffix
  }

  private def renderPositionalParam(param: Parameter): StyledString = {
    val paramName = param.nameOpt
      .filterNot(_ startsWith DesugarHoles.VariableNamePrefix)
      .getOrElse(Parameter.AnonymousParamName)
      .style(getTokenStyle(IDENTIFIER))
    val renderedParam = style"<$paramName>"
    if (param.isVariadic)
      if (param.variadicAtLeastOne)
        style"$renderedParam+..."
      else
        style"$renderedParam..."
    else if (param.hasDefault)
      style"[$renderedParam]"
    else
      renderedParam
  }
}
