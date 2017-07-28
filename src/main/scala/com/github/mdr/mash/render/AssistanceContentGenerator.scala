package com.github.mdr.mash.render

import com.github.mdr.mash.assist.Assistable
import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.functions.{ MashFunction, MashMethod }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.Type.UserClass
import com.github.mdr.mash.screen.StyledString
import com.github.mdr.mash.screen.Style._

case class AssistanceLines(title: String, lines: Seq[StyledString])

object AssistanceContentGenerator {

  def getAssistanceState(assistable: Assistable): AssistanceLines = assistable match {
    case Assistable.Function(f)                ⇒ assistFunction(f)
    case Assistable.FunctionType(f)            ⇒ assistFunction(f)
    case Assistable.Method(m)                  ⇒ assistMethod(m)
    case Assistable.MethodType(m)              ⇒ assistMethod(m)
    case Assistable.ConstructorType(userClass) ⇒ assistClassConstructor(userClass)
  }

  private def assistFunction(f: MashFunction): AssistanceLines = {
    val summaryLineOpt = f.summaryOpt.map(_.style)
    val callingSyntaxLine = CallingSyntaxRenderer.render(f)
    AssistanceLines(
      title = f.name,
      lines = summaryLineOpt.toSeq :+ callingSyntaxLine)
  }

  private def assistFunction(f: Type.UserDefinedFunction) = {
    val Type.UserDefinedFunction(docCommentOpt, _, nameOpt, params, _, _) = f
    val callingSyntaxLine = (nameOpt getOrElse "f").style + " ".style + CallingSyntaxRenderer.render(params)
    val summaryLineOpt = docCommentOpt.map(_.summary.style)
    AssistanceLines(
      title = nameOpt.getOrElse("Anonymous function"),
      lines = summaryLineOpt.toSeq :+ callingSyntaxLine)
  }

  private def assistMethod(method: MashMethod): AssistanceLines = {
    val summaryLineOpt = method.summaryOpt.map(_.style)
    val callingSyntaxLine = CallingSyntaxRenderer.render(method)
    AssistanceLines(
      title = method.name,
      lines = summaryLineOpt.toSeq :+ callingSyntaxLine)
  }

  private def assistMethod(method: Type.UserDefinedFunction): AssistanceLines = {
    val Type.UserDefinedFunction(docCommentOpt, _, nameOpt, params, _, _) = method
    val summaryLineOpt = docCommentOpt.map(_.summary.style)
    val callingSyntaxLine = "target.".style + (nameOpt getOrElse "method").style + " ".style + CallingSyntaxRenderer.render(params)
    AssistanceLines(
      title = nameOpt.getOrElse("Anonymous method"),
      lines = summaryLineOpt.toSeq :+ callingSyntaxLine)
  }

  private def assistClassConstructor(userClass: UserClass): AssistanceLines = {
    val summaryLine = s"Construct a new ${userClass.name} object".style
    val callingSyntaxLine = s"${userClass.name}.${MashClass.ConstructorMethodName} ".style + CallingSyntaxRenderer.render(userClass.params)
    AssistanceLines(
      title = MashClass.ConstructorMethodName,
      lines = Seq(summaryLine, callingSyntaxLine))
  }

}
