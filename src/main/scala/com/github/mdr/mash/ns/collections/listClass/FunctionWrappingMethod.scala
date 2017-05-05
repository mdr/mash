package com.github.mdr.mash.ns.collections.listClass

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, MashMethod }
import com.github.mdr.mash.inference.{ Type, TypedArgument, TypedArguments, ValueInfo }
import com.github.mdr.mash.ns.collections.{ GrepFunction, MaxFunction, MinFunction, ReverseFunction }
import com.github.mdr.mash.runtime.{ MashList, MashValue }

case class FunctionWrappingMethod(function: MashFunction, methodAliases: Seq[String] = Seq()) extends MashMethod(function.name) {

  override def aliases = methodAliases

  private val targetParamName = function match {
    case MinFunction | MaxFunction ⇒ MinFunction.Params.Items.nameOpt.get
    case GrepFunction              ⇒ GrepFunction.Params.Input.nameOpt.get
    case _                         ⇒ ReverseFunction.Params.Sequence.nameOpt.get
  }

  val params = function.params.copy(function.params.params.filterNot(_.nameOpt contains targetParamName))

  def apply(target: MashValue, boundParams: BoundParams): MashValue = {
    val actualTarget = function match {
      case MinFunction | MaxFunction ⇒ MashList.of(target)
      case _                         ⇒ target
    }
    val newBoundNames = boundParams.boundNames + (targetParamName -> actualTarget)
    val newBoundParams = boundParams.copy(boundNames = newBoundNames)
    function.call(newBoundParams)
  }

  override def typeInferenceStrategy = (inferencer, targetTypeOpt, arguments) =>
    function.typeInferenceStrategy.inferTypes(inferencer, updateArgs(arguments, targetTypeOpt))

  override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments): Seq[CompletionSpec] =
    function.getCompletionSpecs(argPos, updateArgs(arguments, targetTypeOpt))

  private def updateArgs(arguments: TypedArguments, targetTypeOpt: Option[Type]): TypedArguments = {
    val sequenceArg = TypedArgument.PositionArg(ValueInfo(None, targetTypeOpt))
    TypedArguments(arguments.arguments :+ sequenceArg)
  }

  override def toString = s"methodise($function)"

  override def summaryOpt = function.summaryOpt

  override def descriptionOpt = function.descriptionOpt

}
