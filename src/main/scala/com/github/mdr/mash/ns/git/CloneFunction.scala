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
import java.io.File

object CloneFunction extends MashFunction("git.clone") {

  object Params {
    val Repository = Parameter(
      name = "repository",
      summary = "Repository to clone")
    val Directory = Parameter(
      name = "directory",
      summary = "Name to give to new repository directory")
  }
  import Params._

  val params = ParameterModel(Seq(Repository, Directory))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val repository = boundParams.validateString(Repository).s
    val directory = boundParams.validateString(Directory).s
    Git.cloneRepository
      .setURI(repository)
      .setDirectory(new File(directory))
      .call()
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def summary = "Clone a repository into a new directory."

}