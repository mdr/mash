package com.github.mdr.mash.render.help

import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.ns.core.help.{ FunctionHelpClass, ParameterHelpClass }
import com.github.mdr.mash.render.MashRenderer
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.screen.Line
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.utils.LineInfo

import scala.collection.mutable.ArrayBuffer

object FunctionHelpRenderer {

  import HelpRenderer._

  def renderFunctionHelp(obj: MashObject): Seq[Line] = {
    import FunctionHelpClass.Fields._
    val help = FunctionHelpClass.Wrapper(obj)
    val lines = ArrayBuffer[Line]()
    lines += Line(SectionTitleStyle(if (help.classOpt.isDefined) "METHOD" else "FUNCTION"))
    val name = help.fullyQualifiedName
    val names = ", ".style.join((name +: help.aliases).map(NameStyle(_)))
    lines += Line(IndentSpace + names + help.summaryOpt.fold("")(" - " + _).style)
    lines += Line.Empty
    for (klass ← help.classOpt) {
      lines += Line(SectionTitleStyle("CLASS"))
      lines += Line(IndentSpace + klass.style)
      lines += Line.Empty
    }
    lines += Line(SectionTitleStyle("CALLING SYNTAX"))
    val maybeTarget = if (help.classOpt.isDefined) "target." else ""
    lines += Line(IndentSpace + maybeTarget.style + obj(CallingSyntax).toString.style)
    lines += Line.Empty
    val parameters = help.parameters
    if (parameters.nonEmpty) {
      lines += Line(SectionTitleStyle("PARAMETERS"))

      for (param ← parameters)
        lines ++= renderParameterHelp(param.asInstanceOf[MashObject])
    }
    for (description ← help.descriptionOpt) {
      lines += Line(SectionTitleStyle("DESCRIPTION"))
      lines ++= DescriptionRenderer.renderDescription(description)
      lines += Line.Empty
    }
    for (source ← help.sourceOpt) {
      lines += Line(SectionTitleStyle("SOURCE"))
      lines ++= renderSource(source)
      lines += Line.Empty
    }
    lines
  }

  private def renderParameterHelp(param: MashObject): Seq[Line] = {
    val lines = ArrayBuffer[Line]()
    val paramHelp = ParameterHelpClass.Wrapper(param)
    var qualifiers: Seq[String] = Seq()
    val isFlag = paramHelp.isFlag
    if (paramHelp.isLazy)
      qualifiers +:= "lazy"
    if (paramHelp.isNamedArgs)
      qualifiers +:= "namedArgs"
    if (paramHelp.isOptional)
      qualifiers +:= "optional"
    if (paramHelp.isVariadic)
      qualifiers +:= "variadic"
    val qualifierString = qualifiers match {
      case Seq() ⇒ ""
      case _     ⇒ qualifiers.mkString(" [", ", ", "]")
    }
    val name = paramHelp.nameOpt getOrElse Parameter.AnonymousParamName
    val paramName = ParamNameStyle(if (isFlag) "--" + name else name)
    val shortFlagDescription = paramHelp.shortFlagOpt.map(f ⇒ s" | -$f").getOrElse("").style(ParamNameStyle)
    lines += Line(IndentSpace + paramName + shortFlagDescription + qualifierString.style + paramHelp.summaryOpt.fold("")(" - " + _).style)
    for (description ← paramHelp.descriptionOpt)
      lines ++= DescriptionRenderer.renderDescription(description, indentLevel = 2)
    lines += Line.Empty
    lines
  }

  private def renderSource(s: String): Seq[Line] = {
    val renderedSource = new MashRenderer().renderChars(s)
    new LineInfo(renderedSource.forgetStyling)
      .lineRegions
      .map(region ⇒ Line(IndentSpace + region.of(renderedSource.chars)))
  }
}
