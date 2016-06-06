package com.github.mdr.mash.ns.os

import java.nio.file.Path
import scala.collection.JavaConverters._
import org.apache.commons.io.FileUtils
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os._
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashList

object ReadLinesFunction extends MashFunction("os.readLines") {

  private val fileSystem = LinuxFileSystem

  object Params {
    val File = Parameter(
      name = "file",
      summary = "File from which to read lines")
  }
  import Params._

  val params = ParameterModel(Seq(File))

  def apply(arguments: Arguments): MashList = {
    val boundParams = params.validate(arguments)
    val path = boundParams.validatePath(File)
    readLines(path)
  }

  def readLines(path: Path): MashList =
    MashList(fileSystem.readLines(path).map(MashString(_)))

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Seq(Type.Instance(StringClass)))

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summary = "Read lines from a file"

  override def descriptionOpt = Some("""Returns a sequence of lines read from the given file.
The default character encoding and line separator are used.""")

}