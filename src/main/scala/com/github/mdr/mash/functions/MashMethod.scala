package com.github.mdr.mash.functions

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.{ Arguments, EvaluationContext }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.MashValue

abstract class MashMethod(val name: String) {

  def apply(target: MashValue, boundParams: BoundParams): MashValue

  def paramContext(target: MashValue): EvaluationContext = EvaluationContext.NotUsed

  def applyNullary(target: MashValue): MashValue = apply(target, params.bindTo(Arguments.EmptyArguments, paramContext(target)))

  def params: ParameterModel

  def allowsNullary: Boolean = params.allowsNullary

  def typeInferenceStrategy: MethodTypeInferenceStrategy = NoMethodTypeInferenceStrategy

  override def toString = s"<.$name>"

  def flags: Seq[Flag] = params.flags

  def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments): Seq[CompletionSpec] = Seq()

  def summaryOpt: Option[String]

  def descriptionOpt: Option[String] = None

  def isPrivate: Boolean = false

  def isPublic: Boolean = !isPrivate

  def aliases: Seq[String] = Seq()

  def names: Seq[String] = name +: aliases

  /**
    * If the method is shy, then it is not made available in subclasses without qualification through "this"
    */
  val isShy: Boolean = false
}