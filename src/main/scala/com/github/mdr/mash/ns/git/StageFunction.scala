package com.github.mdr.mash.ns.git

import scala.collection.JavaConverters._

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type.unitToType
import com.github.mdr.mash.inference.TypedArguments

object StageFunction extends MashFunction("git.stage") {

  object Params {
    val Paths = Parameter(
      name = "paths",
      summary = "Stage the given paths",
      isVariadic = true,
      variadicAtLeastOne = true)
  }
  import Params._

  val params = ParameterModel(Seq(Paths))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val paths = FunctionHelpers.interpretAsPaths(boundParams(Paths))
    GitHelper.withGit { git ⇒
      val status = git.status.call()
      val missing = status.getMissing.asScala
      val (toDelete, toAdd) = paths.partition(p ⇒ missing.contains(p.toString))

      if (toAdd.nonEmpty) {
        val addCommand = git.add
        for (path ← toAdd)
          addCommand.addFilepattern(path.toString)
        addCommand.call()
      }

      if (toDelete.nonEmpty) {
        val rmCommand = git.rm.setCached(true)
        for (path ← toDelete)
          rmCommand.addFilepattern(path.toString)
        rmCommand.call()
      }
    }
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = {
    val status = GitHelper.withGit { _.status.call() }
    val unstagedFiles =
      status.getUntracked.asScala ++ status.getModified.asScala ++ status.getMissing.asScala
    Seq(CompletionSpec.Items(unstagedFiles.toSeq))
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summary = "Stage files"

}