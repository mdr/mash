package com.github.mdr.mash.ns.os

import java.nio.file.Path

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashList, MashString }

object ReadLinesFunction extends MashFunction("os.readLines") {

  private val fileSystem = LinuxFileSystem

  object Params {
    val File = Parameter(
      nameOpt = Some("file"),
      summaryOpt = Some("File from which to read lines"))
  }
  import Params._

  val params = ParameterModel(Seq(File))

  def apply(boundParams: BoundParams): MashList = {
    val path = boundParams.validatePath(File)
    readLines(path)
  }

  def readLines(path: Path): MashList =
    MashList(fileSystem.readLines(path).map(MashString(_)))

  override def typeInferenceStrategy = Type.Seq(StringClass)

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summaryOpt = Some("Read lines from a file")

  override def descriptionOpt = Some("""Returns a sequence of lines read from the given file.
The default character encoding and line separator are used.""")

}