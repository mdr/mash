package com.github.mdr.mash.view.render

import com.github.mdr.mash.assist.Assistable
import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.functions.{ MashFunction, MashMethod }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.Type.UserClass
import com.github.mdr.mash.lexer.TokenType.IDENTIFIER
import com.github.mdr.mash.screen.StyledString
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.view.render.MashRenderer.getTokenStyle

case class BoxContent(title: String, lines: Seq[StyledString])

object AssistanceContentGenerator {

  def getAssistanceState(assistable: Assistable): BoxContent = assistable match {
    case Assistable.Function(f)                ⇒ assistFunction(f)
    case Assistable.FunctionType(f)            ⇒ assistFunction(f)
    case Assistable.Method(m)                  ⇒ assistMethod(m)
    case Assistable.MethodType(m)              ⇒ assistMethod(m)
    case Assistable.ConstructorType(userClass) ⇒ assistClassConstructor(userClass)
  }

  private def assistFunction(f: MashFunction): BoxContent = {
    val summaryLineOpt = f.summaryOpt.map(_.style)
    val callingSyntaxLine = CallingSyntaxRenderer.render(f)
    BoxContent(
      title = f.name,
      lines = summaryLineOpt.toSeq :+ callingSyntaxLine)
  }

  private val identifierStyle = getTokenStyle(IDENTIFIER)

  private def assistFunction(f: Type.UserDefinedFunction) = {
    val Type.UserDefinedFunction(docCommentOpt, _, nameOpt, params, _, _) = f
    val callingSyntaxLine = (nameOpt getOrElse "f").style(identifierStyle.withBold) + " ".style + CallingSyntaxRenderer.render(params)
    val summaryLineOpt = docCommentOpt.map(_.summary.style)
    BoxContent(
      title = nameOpt.getOrElse("Anonymous function"),
      lines = summaryLineOpt.toSeq :+ callingSyntaxLine)
  }

  private def assistMethod(method: MashMethod): BoxContent = {
    val summaryLineOpt = method.summaryOpt.map(_.style)
    val callingSyntaxLine = CallingSyntaxRenderer.render(method)
    BoxContent(
      title = method.name,
      lines = summaryLineOpt.toSeq :+ callingSyntaxLine)
  }

  private def assistMethod(method: Type.UserDefinedFunction): BoxContent = {
    val Type.UserDefinedFunction(docCommentOpt, _, nameOpt, params, _, _) = method
    val summaryLineOpt = docCommentOpt.map(_.summary.style)
    val target = "target".style(identifierStyle)
    val methodName = (nameOpt getOrElse "method").style(identifierStyle.withBold)
    val callingSyntaxLine = style"$target.$methodName ${CallingSyntaxRenderer.render(params)}"
    BoxContent(
      title = nameOpt.getOrElse("Anonymous method"),
      lines = summaryLineOpt.toSeq :+ callingSyntaxLine)
  }

  private def assistClassConstructor(userClass: UserClass): BoxContent = {
    val summaryLine = s"Construct a new ${userClass.name} object".style
    val className = userClass.name.style(identifierStyle)
    val constructorMethodName = MashClass.ConstructorMethodName.style(identifierStyle.withBold)
    val callingSyntaxLine = style"$className.$constructorMethodName ${CallingSyntaxRenderer.render(userClass.params)}"
    BoxContent(
      title = MashClass.ConstructorMethodName,
      lines = Seq(summaryLine, callingSyntaxLine))
  }

}
