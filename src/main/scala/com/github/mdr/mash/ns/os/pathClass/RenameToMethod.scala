package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.{ Files, Path }

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.runtime.{ MashString, MashValue }

import scala.collection.JavaConverters._

object RenameToMethod extends MashMethod("renameTo") {

  object Params {
    val NewName = Parameter(
      nameOpt = Some("newName"),
      summaryOpt = Some("New name for the file or directory"),
      descriptionOpt = Some("""New name must not be "." or "..", contain directory separators, or be empty."""))
  }
  import Params._

  val params = ParameterModel(NewName)

  def call(target: MashValue, boundParams: BoundParams): MashString = {
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

  override def summaryOpt = Some("Rename the file or directory")

  override def descriptionOpt = Some(
    """The file or directory is not moved out of its parent directory.
      |
      |Examples:
      |<mash>
      |  "/etc/hosts.conf".renameTo "hosts.conf.bak" # Moves file to /etc/hosts.conf.bak
      |</mash>
    """.stripMargin)
}