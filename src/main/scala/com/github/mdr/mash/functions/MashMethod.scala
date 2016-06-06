package com.github.mdr.mash.functions

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.inference._
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.runtime.MashValue

abstract class MashMethod(val name: String) {

  def apply(target: MashValue, arguments: Arguments): MashValue

  def params: ParameterModel

  def allowsNullary: Boolean = params.allowsNullary

  def typeInferenceStrategy: MethodTypeInferenceStrategy = NoMethodTypeInferenceStrategy

  override def toString = s"<.$name>"

  def flags: Seq[Flag] = params.flags

  def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments): Seq[CompletionSpec] = Seq()

  def summary: String

  def descriptionOpt: Option[String] = None

}