package com.github.mdr.mash.ns.os

import java.nio.file.Files
import scala.collection.JavaConverters._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter

object MkdirFunction extends MashFunction("mkdir") {

  object Params {
    val Directory = Parameter(
      name = "directory",
      summary = "Path to directory to create")
  }
  import Params._

  val params = ParameterModel(Seq(Directory))

  def apply(arguments: Arguments): MashString = {
    val boundParams = params.validate(arguments)
    val path = boundParams.validatePath(Directory)
    val resultPath = Files.createDirectory(path)
    asPathString(resultPath)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Tagged(StringClass, PathClass))

  override def summary = "Create directory at this path"

}