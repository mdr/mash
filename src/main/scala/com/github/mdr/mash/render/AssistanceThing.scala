package com.github.mdr.mash.render

import com.github.mdr.mash.assist.Assistable
import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.functions.{ MashFunction, MashMethod }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.Type.UserClass

case class AssistanceLines(title: String, lines: Seq[String])

object AssistanceThing {

  def getAssistanceState(assistable: Assistable): AssistanceLines = assistable match {
    case Assistable.Function(f)                ⇒ assistFunction(f)
    case Assistable.FunctionType(f)            ⇒ assistFunction(f)
    case Assistable.Method(m)                  ⇒ assistMethod(m)
    case Assistable.MethodType(m)              ⇒ assistMethod(m)
    case Assistable.ConstructorType(userClass) ⇒ assistClassConstructor(userClass)
  }

  private def assistFunction(f: MashFunction): AssistanceLines =
    AssistanceLines(
      f.name,
      f.summaryOpt.toSeq ++ Seq(
        CallingSyntaxRenderer.render(f).forgetStyling))

  private def assistFunction(f: Type.UserDefinedFunction) = {
    val Type.UserDefinedFunction(docCommentOpt, _, nameOpt, params, _, _) = f
    AssistanceLines(
      nameOpt.getOrElse("Anonymous function"),
      docCommentOpt.map(_.summary).toSeq ++ Seq(
        s"${nameOpt getOrElse "f"} ${CallingSyntaxRenderer.render(params).forgetStyling}"))
  }

  private def assistMethod(method: MashMethod): AssistanceLines =
    AssistanceLines(
      method.name,
      method.summaryOpt.toSeq ++ Seq(CallingSyntaxRenderer.render(method).forgetStyling))

  private def assistMethod(method: Type.UserDefinedFunction): AssistanceLines = {
    val Type.UserDefinedFunction(docCommentOpt, _, nameOpt, params, _, _) = method
    AssistanceLines(
      nameOpt.getOrElse("Anonymous method"),
      docCommentOpt.map(_.summary).toSeq ++ Seq(
        s"target.${nameOpt getOrElse "method"} ${CallingSyntaxRenderer.render(params).forgetStyling}"))
  }

  private def assistClassConstructor(userClass: UserClass): AssistanceLines =
    AssistanceLines(
      MashClass.ConstructorMethodName,
      Seq(
        s"Construct a new ${userClass.name} object",
        s"${userClass.name}.${MashClass.ConstructorMethodName} ${CallingSyntaxRenderer.render(userClass.params).forgetStyling}"))


}
