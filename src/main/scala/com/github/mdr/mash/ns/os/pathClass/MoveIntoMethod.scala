package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.Files

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
      summaryOpt = Some("Location to copy file to"))
  }

  import Params._

  val params = ParameterModel(Seq(Destination))

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    val source = interpretAsPath(target)
    val destination = boundParams.validatePath(Destination)
    if (!Files.isDirectory(destination))
      throw new EvaluatorException(s"Cannot copy into $destination, not a directory")
    val newPath =
      if (Files.isDirectory(source))
        FileUtils.moveDirectoryToDirectory(source.toFile, destination.toFile, false)
      else
        FileUtils.moveFileToDirectory(source.toFile, destination.toFile, false)
    asPathString(destination.resolve(source.getFileName))
  }

  override def typeInferenceStrategy = StringClass taggedWith PathClass

  override def summaryOpt = Some("Move this path into the given directory")

  override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) =
    Seq(CompletionSpec.Directory)

}
