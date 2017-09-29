package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.{ Files, StandardCopyOption }

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ Type, TypedArguments }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.runtime.{ MashString, MashValue }
import org.apache.commons.io.FileUtils

object MoveIntoMethod extends MashMethod("moveInto") {

  object Params {
    val Destination = Parameter(
      nameOpt = Some("destination"),
      summaryOpt = Some("Destination directory to move this file into"))
  }

  import Params._

  val params = ParameterModel(Destination)

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    val source = interpretAsPath(target)
    val destinationDir = boundParams.validatePath(Destination)
    if (!Files.exists(source))
      throw EvaluatorException(s"'$source' does not exist")
    if (!Files.exists(destinationDir))
      boundParams.throwInvalidArgument(Destination, s"'$destinationDir' does not exist")
    if (!Files.isDirectory(destinationDir))
      boundParams.throwInvalidArgument(Destination, s"'$destinationDir' is not a directory")
    val destinationPath = destinationDir.resolve(source.getFileName)
    val createDestDir = false
    if (Files.isDirectory(source))
      FileUtils.moveDirectoryToDirectory(source.toFile, destinationDir.toFile, createDestDir)
    else
      Files.move(source, destinationPath, StandardCopyOption.REPLACE_EXISTING)
    asPathString(destinationPath)
  }

  override def typeInferenceStrategy = StringClass taggedWith PathClass

  override def summaryOpt = Some("Move this path into the given directory")

  override def descriptionOpt = Some("""Returns the new path.""")

  override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) =
    Seq(CompletionSpec.Directory)

}
