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
    val paths = boundParams(Paths).asInstanceOf[MashList]
    LogFunction.withRepository { repo ⇒
      val git = new Git(repo)
      val cmd = git.add
      for (path ← paths)
        cmd.addFilepattern(ToStringifier.stringify(path))
      cmd.call()
    }
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def summary = "Record changes to the repository"

}