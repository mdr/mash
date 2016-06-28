package com.github.mdr.mash.ns.git

import java.io.File

import org.eclipse.jgit.api.Git
import org.eclipse.jgit.transport.URIish

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashString

object CloneFunction extends MashFunction("git.clone") {

  object Params {
    val Repository = Parameter(
      name = "repository",
      summary = "Repository to clone")
    val Directory = Parameter(
      name = "directory",
      summary = "Name to give to new repository directory",
      defaultValueGeneratorOpt = Some(() ⇒ MashNull))
  }
  import Params._

  val params = ParameterModel(Seq(Repository, Directory))

  def apply(arguments: Arguments): MashString = {
    val boundParams = params.validate(arguments)
    val repository = boundParams.validateString(Repository).s
    val directory = boundParams.validateStringOpt(Directory).map(_.s).getOrElse(new URIish(repository).getHumanishName)
    val cmd = Git.cloneRepository
    cmd.setURI(repository)
    cmd.setCloneAllBranches(true)
    cmd.setNoCheckout(false)
    cmd.setDirectory(new File(directory))
    val git = cmd.call()
    val path =
      try
        git.getRepository.getWorkTree
      finally
        git.close()
    FunctionHelpers.asPathString(path)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(StringClass taggedWith PathClass)

  override def summary = "Clone a repository into a new directory."

}