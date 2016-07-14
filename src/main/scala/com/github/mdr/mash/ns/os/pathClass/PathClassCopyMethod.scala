package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.Files

import org.apache.commons.io.FileUtils

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions._
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.runtime.MashUnit
import com.github.mdr.mash.runtime.MashValue

object PathClassCopyMethod extends MashMethod("copy") {

  object Params {
    val Destination = Parameter(
      name = "destination",
      summary = "Location to copy file to")
  }
  import Params._

  val params = ParameterModel(Seq(Destination))

  def apply(target: MashValue, arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
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

  override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Unit)

  override def summary = "Copy this file or directory to another location"

}