package com.github.mdr.mash.functions

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.inference._
import com.github.mdr.mash.evaluator.Arguments

case class Namespace(segments: Seq[String]) {

  override def toString = segments.mkString(".")

}

trait HasName {

  def nameOpt: Option[String]

  def namespaceOpt: Option[Namespace]

  def name = nameOpt.getOrElse("anonymous")

  def segments: Seq[String] = namespaceOpt.toSeq.flatMap(_.segments) ++ nameOpt
  
  def fullyQualifiedName: String =
    namespaceOpt match {
      case Some(namespace) ⇒ s"$namespace.$name"
      case None            ⇒ name
    }
}

abstract class MashFunction(val nameOpt: Option[String], val namespaceOpt: Option[Namespace] = None) extends HasName {

  def this(s: String) = this(s.split("\\.").lastOption, Some(Namespace(s.split("\\.").init)))

  def apply(arguments: Arguments): Any

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

  override def toString = fullyQualifiedName

}