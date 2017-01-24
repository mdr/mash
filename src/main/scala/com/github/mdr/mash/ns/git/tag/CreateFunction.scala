package com.github.mdr.mash.ns.git.tag

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.git.GitHelper
import com.github.mdr.mash.runtime.MashObject

object CreateFunction extends MashFunction("git.tag.create") {

  object Params {
    lazy val Name: Parameter = Parameter(
      nameOpt = Some("name"),
      summaryOpt = Some("Name to give the tag"))
  }

  import Params._

  val params = ParameterModel(Seq(Name))

  def apply(arguments: Arguments): MashObject = {
    val boundParams = params.validate(arguments)
    val name = boundParams.validateString(Name).s

    GitHelper.withGit { git ⇒
      val ref = git.tag().setName(name).call()
      ListFunction.asMashObject(ref)
    }
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(TagClass)

  override def summaryOpt = Some("Create a Git tag")
}
