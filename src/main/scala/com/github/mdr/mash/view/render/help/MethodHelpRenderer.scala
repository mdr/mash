package com.github.mdr.mash.view.render.help

import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.ns.core.help.MethodHelpClass
import com.github.mdr.mash.view.render.CallingSyntaxRenderer
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Line

object MethodHelpRenderer extends AbstractHelpRenderer {

  def render(obj: MashObject): LinesAndLinks = {
    val help = MethodHelpClass.Wrapper(obj)
    val method = help.method
    LinesAndLinks.combine(Seq(
      renderNameSection(method),
      renderClassSection(help.klass),
      LinesAndLinks(renderCallingSyntaxSection(help)),
      LinesAndLinks(ParameterHelpRenderer.renderSection(method.params.params)),
      renderDescriptionSection(method.descriptionOpt),
      LinesAndLinks(FunctionHelpRenderer.renderSourceSection(method.sourceOpt))))
  }

  private def renderCallingSyntaxSection(help: MethodHelpClass.Wrapper): Seq[Line] =
    if (help.method.params.isEmpty)
      Seq()
    else
      FunctionHelpRenderer.renderCallingSyntaxSection(CallingSyntaxRenderer.render(help.method, Some(help.klass)))

  private def renderNameSection(method: MashMethod): LinesAndLinks =
    renderNameSection("METHOD", method.name +: method.aliases, method.summaryOpt)

}
