package com.github.mdr.mash.functions

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.inference._
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.evaluator.SourceLocation

case class ArgumentException(message: String, locationOpt: Option[SourceLocation] = None) extends RuntimeException(message)

abstract class MashFunction(
    val nameOpt: Option[String] = None,
    val namespaceOpt: Option[Namespace] = None) extends MashValue with HasName with MashCallable {

  def this(s: String) = this(s.split("\\.").lastOption, Some(Namespace(s.split("\\.").init)))

  @throws[ArgumentException]
  def apply(arguments: Arguments): MashValue

  /**
   * When true, it is possible to call this function with no arguments
   */
  def allowsNullary: Boolean = params.allowsNullary

  def params: ParameterModel

  def typeInferenceStrategy: TypeInferenceStrategy = NoTypeInferenceStrategy

  def flags: Seq[Flag] = params.flags

  def getCompletionSpecs(argPos: Int, arguments: TypedArguments): Seq[CompletionSpec] = Seq()

  def summary: String

  def descriptionOpt: Option[String] = None

  override def toString = fullyQualifiedName.toString

}