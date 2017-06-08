package com.github.mdr.mash.ns.xml

import java.nio.charset.StandardCharsets

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.ns.json
import com.github.mdr.mash.runtime._
import org.apache.commons.io.FileUtils
import org.json.XML

object ReadFunction extends MashFunction("xml.read") {

  object Params {
    val File = Parameter(
      nameOpt = Some("file"),
      summaryOpt = Some("File from which to read an XML document"))
  }

  import Params._

  val params = ParameterModel(File)

  def call(boundParams: BoundParams): MashValue = {
    val path = boundParams.validatePath(File)
    val xml = FileUtils.readFileToString(path.toFile, StandardCharsets.UTF_8)
    fromString(xml)
  }

  def fromString(xml: String): MashValue = {
    val jsonText = XML.toJSONObject(xml).toString(2)
    json.ReadFunction.parseJson(jsonText)
  }


  override def typeInferenceStrategy = ObjectClass

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summaryOpt = Some("Read the given file and parse it as XML")

}