package com.github.mdr.mash.ns.git

import org.eclipse.jgit.api.Git
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.evaluator.MashList
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.functions.FunctionHelpers
import java.nio.file.Path
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.completions.CompletionSpec

object AddFunction extends MashFunction("git.add") {

  object Params {
    val Paths = Parameter(
      name = "paths",
      summary = "Add paths to the index",
      isVariadic = true,
      variadicAtLeastOne = true)
  }
  import Params._

  val params = ParameterModel(Seq(Paths))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val argItems = boundParams(Paths).asInstanceOf[MashList].items
    val paths = FunctionHelpers.interpretAsPaths(boundParams(Paths))
    GitHelper.withGit { git ⇒
      val cmd = git.add
      for (path ← paths)
        cmd.addFilepattern(path.toString)
      cmd.call()
    }
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def summary = "Record changes to the repository"

}