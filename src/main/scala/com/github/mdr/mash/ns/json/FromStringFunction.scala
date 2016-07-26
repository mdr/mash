package com.github.mdr.mash.ns.json

import java.nio.charset.StandardCharsets
import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap
import org.apache.commons.io.FileUtils
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.google.gson._
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashBoolean
import com.github.mdr.mash.evaluator.ToStringifier

object FromStringFunction extends MashFunction("json.fromString") {

  object Params {
    val String = Parameter(
      name = "string",
      summary = "String to parse as JSON")
  }
  import Params._

  val params = ParameterModel(Seq(String))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val s = boundParams.validateString(String).s
    FromFileFunction.parseJson(s)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Any)

  override def summary = "Read the given string and parse it as JSON"
}