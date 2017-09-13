package com.github.mdr.mash.ns.os

import java.nio.file.Path

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.os.linux.{ LinuxEnvironmentInteractions, LinuxFileSystem }
import com.github.mdr.mash.runtime.{ MashString, MashUnit }

object ChangeDirectoryFunction extends MashFunction("os.changeDirectory") {

  sealed trait Result

  case object Success extends Result

  case object NotADirectory extends Result

  private val fileSystem = LinuxFileSystem
  private val environmentInteractions = LinuxEnvironmentInteractions
  private val workingDirectoryStack = Singletons.workingDirectoryStack

  object Params {
    val Directory = Parameter(
      nameOpt = Some("directory"),
      summaryOpt = Some("Directory to change into; defaults to the current user's home directory."),
      defaultValueGeneratorOpt = Some(home))
  }

  import Params._

  val params = ParameterModel(Directory)

  def call(boundParams: BoundParams): MashUnit = {
    val path = boundParams.validatePath(Directory)
    changeDirectory(path) match {
      case Success       ⇒
        MashUnit
      case NotADirectory ⇒
        boundParams.throwInvalidArgument(Directory, s"Could not change directory to '$path', not a directory")
    }
  }

  private def home = MashString(environmentInteractions.home.toString, Some(PathClass))

  def changeDirectory(path: Path): Result = {
    val result =
      if (fileSystem.isDirectory(path)) {
        fileSystem.chdir(path)
        Success
      } else
        NotADirectory
    workingDirectoryStack.push(fileSystem.pwd)
    result
  }

  override def typeInferenceStrategy = Unit

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.Directory)

  override def summaryOpt = Some("Change the current working directory")

  override def descriptionOpt = Some(
    """Examples:
<mash>
   cd "/tmp"
   cd # cd to home directory
</mash>""")

}