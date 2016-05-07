package com.github.mdr.mash.completions

import com.github.mdr.mash.evaluator.BoundMethod
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.utils.Region

object BindingCompleter {

  def completeBindings(env: Environment, prefix: String, region: Region): Option[CompletionResult] = {
    val completions =
      for {
        (name, value) ← env.valuesMap.toSeq
        if name startsWith prefix
        (completionType, description) = getBindingTypeAndDescription(value)
      } yield Completion(name, typeOpt = Some(completionType), descriptionOpt = Some(description))
    CompletionResult.of(completions, region)
  }

  private def getBindingTypeAndDescription(value: Any): (CompletionType, String) = value match {
    case mf: MashFunction ⇒ (CompletionType.Function, mf.summary)
    case bf: BoundMethod  ⇒ (CompletionType.Method, bf.method.summary)
    case x                ⇒ (CompletionType.Binding, ToStringifier.stringify(x))
  }

}