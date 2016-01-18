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

object ReadLinesFunction extends MashFunction("os.readLines") {

  private val fileSystem = LinuxFileSystem

  private val File = "file"

  val params = ParameterModel(Seq(
    Parameter(
      name = File,
      summary = "File from which to read lines")))

  def readLines(path: Path): Seq[MashString] =
    fileSystem.readLines(path).map(MashString(_))

  def apply(arguments: Arguments): Seq[MashString] = {
    val boundParams = params.validate(arguments)
    val path = FunctionHelpers.interpretAsPath(boundParams(File))
    readLines(path)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Seq(Type.Instance(StringClass)))

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summary = "Read lines from a file"

  override def descriptionOpt = Some("""Returns a sequence of lines read from the given file.
The default character encoding and line separator are used.""")

}