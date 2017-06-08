package com.github.mdr.mash.ns.git

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime.MashUnit

object AddFunction extends MashFunction("git.add") {

  object Params {
    val Paths = Parameter(
      nameOpt = Some("paths"),
      summaryOpt = Some("Add paths to the index"),
      isVariadic = true,
      variadicAtLeastOne = true,
      variadicFlatten = true)
  }
  import Params._

  val params = ParameterModel(Paths)

  def call(boundParams: BoundParams): MashUnit = {
    val paths = FunctionHelpers.interpretAsPaths(boundParams(Paths))
    GitHelper.withGit { git ⇒
      val cmd = git.add
      for (path ← paths)
        cmd.addFilepattern(path.toString)
      cmd.call()
    }
    MashUnit
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Record changes to the repository")

}