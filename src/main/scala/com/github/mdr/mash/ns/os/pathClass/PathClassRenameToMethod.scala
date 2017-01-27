package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.{ Files, Path }

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.runtime.{ MashString, MashValue }

import scala.collection.JavaConverters._

object PathClassRenameToMethod extends MashMethod("renameTo") {

  object Params {
    val NewName = Parameter(
      nameOpt = Some("newName"),
      summaryOpt = Some("New name for the file or directory"),
      descriptionOpt = Some("""New name must be a simple name (not a path with directory separators)"""))
  }
  import Params._

  val params = ParameterModel(Seq(NewName))

  def apply(target: MashValue, arguments: Arguments): MashString = {
    val boundParams = params.validate(arguments)
    val path = FunctionHelpers.interpretAsPath(target)
    val newName = validateName(boundParams, NewName)
    val newPath = path.resolveSibling(newName)
    val newLocation = Files.move(path, newPath)
    asPathString(newLocation)
  }

  private def validateName(boundParams: BoundParams, param: Parameter): Path = {
    val newName = boundParams.validatePath(param)
    validateName(newName, boundParams, param)
  }

  def validateName(newName: Path, boundParams: BoundParams, param: Parameter): Path =
    if (newName.asScala.size > 1)
      boundParams.throwInvalidArgument(param, "Name cannot contain directory separators")
    else if (newName.toString == "")
      boundParams.throwInvalidArgument(param, "Name cannot be empty")
    else if (newName.toString == "." || newName.toString == "..")
      boundParams.throwInvalidArgument(param, "Name cannot be . or ..")
    else
      newName

  override def typeInferenceStrategy = StringClass taggedWith PathClass

  override def summaryOpt = Some("Rename the file or directory at this path")

}