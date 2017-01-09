package com.github.mdr.mash.ns.os

import java.nio.file.Path

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantTypeInferenceStrategy, TypedArguments }
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
      summary = "Directory to change into; defaults to the current user's home directory.",
      defaultValueGeneratorOpt = Some(() ⇒ home))
  }
  import Params._

  val params = ParameterModel(Seq(Directory))

  def apply(arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
    val path = boundParams.validatePath(Directory)
    changeDirectory(path) match {
      case Success ⇒
        MashUnit
      case NotADirectory ⇒
        boundParams.throwInvalidArgument(Directory, s"Could not change directory to '$path', not a directory")
    }
  }

  private def home = MashString(environmentInteractions.home.toString, Some(PathClass))

  def changeDirectory(path: Path): Result = {
    workingDirectoryStack.push(fileSystem.pwd)
    if (fileSystem.isDirectory(path)) {
      fileSystem.chdir(path)
      Success
    } else
      NotADirectory
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.Directory)

  override def summary = "Change the current working directory"

  override def descriptionOpt = Some("""Examples:
   cd "/tmp"
   cd # cd to home directory""")

}