package com.github.mdr.mash.render.help

import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.ns.core.help.{ HelpCreator, MethodHelpClass }
import com.github.mdr.mash.render.CallingSyntaxRenderer
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Line

object MethodHelpRenderer extends AbstractHelpRenderer {

  def render(obj: MashObject): Seq[Line] = {
    val help = MethodHelpClass.Wrapper(obj)
    val method = help.method
    val paramHelp = method.params.params.map(HelpCreator.getParamHelp)
    Seq(
      renderNameSection(method),
      renderClassSection(help.klass),
      FunctionHelpRenderer.renderCallingSyntaxSection(CallingSyntaxRenderer.render(method, Some(help.klass))),
      ParameterHelpRenderer.renderSection(paramHelp),
      renderDescriptionSection(method.descriptionOpt),
      FunctionHelpRenderer.renderSourceSection(method.sourceOpt)).flatten
  }

  private def renderNameSection(method: MashMethod): Seq[Line] = {
    val names = method.name +: method.aliases
    renderNameSection("METHOD", names, method.summaryOpt)
  }

}
