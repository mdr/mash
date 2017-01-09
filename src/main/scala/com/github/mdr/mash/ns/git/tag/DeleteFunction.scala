package com.github.mdr.mash.ns.git.tag

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantTypeInferenceStrategy, TypedArguments }
import com.github.mdr.mash.ns.git.GitHelper
import com.github.mdr.mash.ns.git.branch.{ BranchClass, SwitchFunction }
import com.github.mdr.mash.runtime.{ MashObject, MashString, MashUnit, MashValue }

object DeleteFunction extends MashFunction("git.tag.delete") {

  object Params {
    val Tags = Parameter(
      nameOpt = Some("tags"),
      summary = "Tags to delete",
      isVariadic = true,
      variadicAtLeastOne = true)
  }

  import Params._

  val params = ParameterModel(Seq(Tags))

  def validateTag(boundParams: BoundParams, param: Parameter, tag: MashValue): String = {
    val tagName = tag match {
      case MashString(s, _)                  ⇒ s
      case obj@MashObject(_, Some(TagClass)) ⇒ TagClass.Wrapper(obj).name.s
      case _                                 ⇒ boundParams.throwInvalidArgument(param, "Must be a tag")
    }
    if (!ListFunction.getTagNames.contains(tagName))
      boundParams.throwInvalidArgument(param, "Must be a tag")
    tagName
  }

  def validateTags(boundParams: BoundParams, param: Parameter): Seq[String] =
    boundParams.validateSequence(param).map(tag ⇒ validateTag(boundParams, param, tag))

  def apply(arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
    val tags = validateTags(boundParams, Tags)
    GitHelper.withGit { git ⇒
      git.tagDelete.setTags(tags: _*).call()
    }
    MashUnit
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Tags ⇒ CompletionSpec.Items(ListFunction.getTagNames)
    }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summary = "Delete a tag"
}
