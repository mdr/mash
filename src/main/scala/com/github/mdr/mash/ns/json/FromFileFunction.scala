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
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.completions.CompletionSpec

object FromFileFunction extends MashFunction("json.fromFile") {

  private val filesystem = LinuxFileSystem

  object Params {
    val File = Parameter(
      name = "file",
      summary = "File from which to read lines")
  }
  import Params._

  val params = ParameterModel(Seq(File))

  def apply(arguments: Arguments): Any = {
    val boundParams = params.validate(arguments)
    val path = boundParams.validatePath(File)
    val parser = new JsonParser

    val json = parser.parse(FileUtils.readFileToString(path.toFile, StandardCharsets.UTF_8))
    asMashObject(json)
  }

  private def asMashObject(e: JsonElement): Any = e match {
    case _: JsonNull      ⇒ null
    case array: JsonArray ⇒ MashList(array.iterator.asScala.toSeq.map(asMashObject))
    case p: JsonPrimitive ⇒
      if (p.isNumber) MashNumber(p.getAsDouble)
      else if (p.isBoolean) p.getAsBoolean
      else if (p.isString) MashString(p.getAsString)
      else throw new EvaluatorException("Unknown primitive in JSON: " + p)
    case obj: JsonObject ⇒
      val fields =
        for (x ← obj.entrySet.iterator.asScala.toSeq)
          yield x.getKey -> asMashObject(x.getValue)
      MashObject(ListMap(fields: _*), classOpt = None)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Any)

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summary = "Read the given file and parse it as JSON"
}