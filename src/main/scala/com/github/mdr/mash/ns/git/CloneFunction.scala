package com.github.mdr.mash.ns.git

import java.io.File

import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.runtime.{ MashNull, MashString }
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.transport.URIish

object CloneFunction extends MashFunction("git.clone") {

  object Params {
    val Repository = Parameter(
      nameOpt = Some("repository"),
      summaryOpt = Some("Repository URL to clone"))
    val Directory = Parameter(
      nameOpt = Some("directory"),
      summaryOpt = Some("Name to give to new repository directory"),
      defaultValueGeneratorOpt = Some(MashNull))
  }
  import Params._

  val params = ParameterModel(Seq(Repository, Directory))

  def apply(boundParams: BoundParams): MashString = {
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

  override def typeInferenceStrategy = StringClass taggedWith PathClass

  override def summaryOpt = Some("Clone a Git repository into a new directory.")

  override def descriptionOpt = Some("""Returns the path to the new directory.
    
Examples:
  git.clone "https://github.com/github/testrepo.git"     # clones into "testRepo"
  git.clone "https://github.com/github/testrepo.git" dir # clones into "dir"""")
}