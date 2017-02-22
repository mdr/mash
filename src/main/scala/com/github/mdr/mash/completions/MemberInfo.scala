package com.github.mdr.mash.completions

case class MemberInfo(name: String,
                      isField: Boolean,
                      descriptionOpt: Option[String] = None,
                      classNameOpt: Option[String] = None,
                      isVectorised: Boolean = false,
                      isStatic: Boolean = false) {

  def isMethod = !isField

  private def description: String = {
    val vecPrefix = if (isVectorised) "vectorised, " else ""
    val fieldOrMethod = if (isField) "field" else if (isStatic) "static method" else "method"
    val classOrigin = classNameOpt.map(" in " + _).getOrElse("")
    val desc = descriptionOpt.getOrElse(name)
    s"$desc ($vecPrefix$fieldOrMethod$classOrigin)"
  }

  private def completionType: CompletionType = if (isField) CompletionType.Field else CompletionType.Method

  def asCompletion(isQuoted: Boolean) =
    Completion(name, isQuoted = isQuoted, typeOpt = Some(completionType), descriptionOpt = Some(description))

}