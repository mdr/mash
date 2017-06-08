package com.github.mdr.mash.ns.os

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.MashString

object ReadFunction extends MashFunction("os.read") {

  private val fileSystem = LinuxFileSystem

  object Params {
    val File = Parameter(
      nameOpt = Some("file"),
      summaryOpt = Some("File from which to read text"))
  }
  import Params._

  val params = ParameterModel(File)

  def call(boundParams: BoundParams): MashString = {
    val path = boundParams.validatePath(File)
    MashString(fileSystem.read(path))
  }

  override def typeInferenceStrategy = StringClass

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summaryOpt = Some("Read the contents of a file as a string")

  override def descriptionOpt = Some("""Returns a string with the contents of the given file.
The default character encoding is used.""")

}