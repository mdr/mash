package com.github.mdr.mash.ns.os

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.os.CurrentDirectoryManager._
import com.github.mdr.mash.os.{ CurrentDirectoryManager, EnvironmentInteractions }
import com.github.mdr.mash.runtime.{ MashString, MashUnit }

case class ChangeDirectoryFunction(currentDirectoryManager: CurrentDirectoryManager,
                                   environmentInteractions: EnvironmentInteractions)
  extends MashFunction("os.changeDirectory") {

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
    currentDirectoryManager.changeDirectory(path) match {
      case Success       ⇒ MashUnit
      case NotADirectory ⇒ boundParams.throwInvalidArgument(Directory, s"Could not change directory to '$path', not a directory")
    }
  }

  private def home = MashString(environmentInteractions.home.toString, Some(PathClass))

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