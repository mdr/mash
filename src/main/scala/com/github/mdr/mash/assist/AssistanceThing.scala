package com.github.mdr.mash.assist

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.functions.{ MashFunction, MashMethod }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.Type.UserClass
import com.github.mdr.mash.render.CallingSyntaxRenderer

object AssistanceThing {

  def getAssistanceState(assistable: Assistable): AssistanceState = assistable match {
    case Assistable.Function(f)                ⇒ assistFunction(f)
    case Assistable.FunctionType(f)            ⇒ assistFunction(f)
    case Assistable.Method(m)                  ⇒ assistMethod(m)
    case Assistable.MethodType(m)              ⇒ assistMethod(m)
    case Assistable.ConstructorType(userClass) ⇒ assistClassConstructor(userClass)
  }

  private def assistFunction(f: MashFunction): AssistanceState =
    AssistanceState(
      f.name,
      f.summaryOpt.toSeq ++ Seq(
        CallingSyntaxRenderer.render(f).forgetStyling))

  private def assistFunction(f: Type.UserDefinedFunction) = {
    val Type.UserDefinedFunction(docCommentOpt, _, nameOpt, params, _, _) = f
    AssistanceState(
      nameOpt.getOrElse("Anonymous function"),
      docCommentOpt.map(_.summary).toSeq ++ Seq(
        s"${nameOpt getOrElse "f"} ${CallingSyntaxRenderer.render(params).forgetStyling}"))
  }

  private def assistMethod(method: MashMethod): AssistanceState =
    AssistanceState(
      method.name,
      method.summaryOpt.toSeq ++ Seq(CallingSyntaxRenderer.render(method).forgetStyling))

  private def assistMethod(method: Type.UserDefinedFunction): AssistanceState = {
    val Type.UserDefinedFunction(docCommentOpt, _, nameOpt, params, _, _) = method
    AssistanceState(
      nameOpt.getOrElse("Anonymous method"),
      docCommentOpt.map(_.summary).toSeq ++ Seq(
        s"target.${nameOpt getOrElse "method"} ${CallingSyntaxRenderer.render(params).forgetStyling}"))
  }

  private def assistClassConstructor(userClass: UserClass): AssistanceState =
    AssistanceState(
      MashClass.ConstructorMethodName,
      Seq(
        s"Construct a new ${userClass.name} object",
        s"${userClass.name}.${MashClass.ConstructorMethodName} ${CallingSyntaxRenderer.render(userClass.params).forgetStyling}"))


}
