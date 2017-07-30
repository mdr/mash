package com.github.mdr.mash.render.help

import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.ns.core.help.MethodHelpClass
import com.github.mdr.mash.render.CallingSyntaxRenderer
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Line

object MethodHelpRenderer extends AbstractHelpRenderer {

  def render(obj: MashObject): Seq[Line] = {
    val help = MethodHelpClass.Wrapper(obj)
    val method = help.method
    Seq(
      renderNameSection(method),
      renderClassSection(help.klass),
      renderCallingSyntaxSection(help),
      ParameterHelpRenderer.renderSection(method.params.params),
      renderDescriptionSection(method.descriptionOpt),
      FunctionHelpRenderer.renderSourceSection(method.sourceOpt)).flatten
  }

  private def renderCallingSyntaxSection(help: MethodHelpClass.Wrapper): Seq[Line] =
    if (help.method.params.isEmpty)
      Seq()
    else
      FunctionHelpRenderer.renderCallingSyntaxSection(CallingSyntaxRenderer.render(help.method, Some(help.klass)))

  private def renderNameSection(method: MashMethod): Seq[Line] =
    renderNameSection("METHOD", method.name +: method.aliases, method.summaryOpt)

}
