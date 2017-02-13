package com.github.mdr.mash.ns.xml

import java.nio.charset.StandardCharsets

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.runtime._
import org.apache.commons.io.FileUtils
import org.json.XML
import com.github.mdr.mash.ns.json

object FromFileFunction extends MashFunction("xml.fromFile") {

  object Params {
    val File = Parameter(
      nameOpt = Some("file"),
      summaryOpt = Some("File from which to read an XML document"))
  }

  import Params._

  val params = ParameterModel(Seq(File))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val path = boundParams.validatePath(File)
    val s = FileUtils.readFileToString(path.toFile, StandardCharsets.UTF_8)
    val jsonText = XML.toJSONObject(s).toString(4)
    json.FromFileFunction.parseJson(jsonText)
  }

  override def typeInferenceStrategy = ObjectClass

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summaryOpt = Some("Read the given file and parse it as XML")

}