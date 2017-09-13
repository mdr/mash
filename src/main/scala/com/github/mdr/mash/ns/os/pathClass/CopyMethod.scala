package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.Files

import com.github.mdr.mash.functions.{ ParameterModel, _ }
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime.{ MashUnit, MashValue }
import org.apache.commons.io.FileUtils

object CopyMethod extends MashMethod("copy") {

  object Params {
    val Destination = Parameter(
      nameOpt = Some("destination"),
      summaryOpt = Some("Location to copy file to"))
  }
  import Params._

  val params = ParameterModel(Destination)

  def call(target: MashValue, boundParams: BoundParams): MashUnit = {
    val source = FunctionHelpers.interpretAsPath(target)
    val destination = boundParams.validatePath(Destination)
    if (Files.isDirectory(source))
      if (Files.exists(destination))
        boundParams.throwInvalidArgument(Destination, "Destination already exists")
      else
        FileUtils.copyDirectory(source.toFile, destination.toFile)
    else {
      if (Files.exists(destination) && Files.isDirectory(destination))
        boundParams.throwInvalidArgument(Destination, "Destination already exists, and is a directory")
      else
        Files.copy(source, destination)
    }
    MashUnit
  }

  override def typeInferenceStrategy = Unit

  override def summaryOpt = Some("Copy this file or directory to another location")

}