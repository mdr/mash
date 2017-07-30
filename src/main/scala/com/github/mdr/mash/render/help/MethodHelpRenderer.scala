package com.github.mdr.mash.render.help

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.ns.core.help.{ HelpCreator, MethodHelpClass }
import com.github.mdr.mash.render.CallingSyntaxRenderer
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.screen.Style._

object MethodHelpRenderer {

  import HelpRenderer._

  def render(obj: MashObject): Seq[Line] = {
    val help = MethodHelpClass.Wrapper(obj)
    val method = help.method
    val paramHelp = method.params.params.map(HelpCreator.getParamHelp)
    Seq(
      renderNameSection(method),
      renderClassSection(help.klass),
      FunctionHelpRenderer.renderCallingSyntaxSection(CallingSyntaxRenderer.render(method)),
      ParameterHelpRenderer.renderSection(paramHelp),
      FunctionHelpRenderer.renderDescriptionSection(method.descriptionOpt),
      FunctionHelpRenderer.renderSourceSection(method.sourceOpt)).flatten
  }

  private def renderNameSection(method: MashMethod): Seq[Line] = {
    val names = method.name +: method.aliases
    FunctionHelpRenderer.renderNameSection("METHOD", names, method.summaryOpt)
  }

  private def renderClassSection(klass: MashClass): Seq[Line] =
    Seq(
      Line.Empty,
      Line(SectionTitleStyle("CLASS")),
      Line(IndentSpace + klass.fullyQualifiedName.toString.style))

}
