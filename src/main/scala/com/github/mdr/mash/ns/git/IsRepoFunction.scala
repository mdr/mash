package com.github.mdr.mash.ns.git

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.MashBoolean

object IsRepoFunction extends MashFunction("git.isRepo") {

  private val fileSystem = LinuxFileSystem

  object Params {
    val Dir = Parameter(
      nameOpt = Some("dir"),
      summaryOpt = Some("Directory to test"))
  }
  import Params._

  val params = ParameterModel(Seq(Dir))

  def apply(boundParams: BoundParams): MashBoolean = {
    val path = boundParams.validatePath(Dir)
    MashBoolean(GitHelper.isRepository(path))
  }

  override def typeInferenceStrategy = BooleanClass

  override def summaryOpt = Some("Return true if a directory is within a Git repository")

}