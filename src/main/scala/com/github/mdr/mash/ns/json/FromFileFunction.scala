package com.github.mdr.mash.ns.json

import java.nio.charset.StandardCharsets

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.runtime._
import com.google.gson._
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._

object FromFileFunction extends MashFunction("json.fromFile") {

  object Params {
    val File = Parameter(
      nameOpt = Some("file"),
      summaryOpt = Some("File from which to read JSON"))
  }
  import Params._

  val params = ParameterModel(Seq(File))

  def apply(boundParams: BoundParams): MashValue = {
    val path = boundParams.validatePath(File)
    val s = FileUtils.readFileToString(path.toFile, StandardCharsets.UTF_8)
    parseJson(s)
  }

  def parseJson(s: String): MashValue = {
    val json = new JsonParser().parse(s)
    asMashObject(json)
  }

  private def asMashObject(e: JsonElement): MashValue = e match {
    case _: JsonNull      ⇒ MashNull
    case array: JsonArray ⇒ MashList(array.iterator.asScala.toSeq.map(asMashObject))
    case p: JsonPrimitive ⇒
      if (p.isNumber) MashNumber(p.getAsDouble)
      else if (p.isBoolean) MashBoolean(p.getAsBoolean)
      else if (p.isString) MashString(p.getAsString)
      else throw new EvaluatorException("Unknown primitive in JSON: " + p)
    case obj: JsonObject ⇒
      val fields =
        for (x ← obj.entrySet.iterator.asScala.toSeq)
          yield x.getKey -> asMashObject(x.getValue)
      MashObject.of(fields)
  }

  override def typeInferenceStrategy = AnyClass

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summaryOpt = Some("Read the given file and parse it as JSON")
}