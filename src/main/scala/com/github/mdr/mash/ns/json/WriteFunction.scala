package com.github.mdr.mash.ns.json

import java.nio.charset.StandardCharsets

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.{ AnyClass, UnitClass }
import com.github.mdr.mash.runtime._
import com.google.gson._
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._

object WriteFunction extends MashFunction("json.write") {

  object Params {
    val File = Parameter(
      nameOpt = Some("file"),
      summaryOpt = Some("File to write JSON to"))
    val Value = Parameter(
      nameOpt = Some("value"),
      summaryOpt = Some("Value to write"))
  }

  import Params._

  val params = ParameterModel(Seq(File, Value))

  def call(boundParams: BoundParams): MashUnit = {
    val path = boundParams.validatePath(File)
    val value = boundParams(Value)
    val contents = PrettyPrintFunction.asJsonString(value)
    FileUtils.writeStringToFile(path.toFile, contents, StandardCharsets.UTF_8)
    MashUnit
  }

  override def typeInferenceStrategy = UnitClass

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summaryOpt = Some("Write the given value as JSON to the given file")

}