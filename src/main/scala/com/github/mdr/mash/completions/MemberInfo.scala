package com.github.mdr.mash.completions

import com.github.mdr.mash.classes.MashClass

case class MemberInfo(name: String,
                      isField: Boolean,
                      descriptionOpt: Option[String] = None,
                      classOpt: Option[MashClass] = None,
                      isVectorised: Boolean = false) {

  def isMethod = !isField

  private def description: String = {
    val vecPrefix = if (isVectorised) "vectorised, " else ""
    val fieldOrMethod = if (isField) "field" else "method"
    val classOrigin = classOpt.flatMap(_.nameOpt).map(" in " + _).getOrElse("")
    val desc = descriptionOpt.getOrElse("")
    s"$desc ($vecPrefix$fieldOrMethod$classOrigin)"
  }

  private def completionType: CompletionType = if (isField) CompletionType.Field else CompletionType.Method

  def asCompletion(isQuoted: Boolean) =
    Completion(name, isQuoted = isQuoted, typeOpt = Some(completionType), descriptionOpt = Some(description))

}