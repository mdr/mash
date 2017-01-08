package com.github.mdr.mash.ns.os

import java.nio.charset.StandardCharsets

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.{ MashBoolean, MashList, MashUnit }
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._

object WriteFunction extends MashFunction("os.write") {

  object Params {
    val Append = Parameter(
      name = "append",
      summary = "Append to the end of the file, if it already exists",
      shortFlagOpt = Some('a'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isBooleanFlag = true)
    val File = Parameter(
      name = "file",
      summary = "File to write to")
    val Data = Parameter(
      name = "data",
      summary = "Data to write to the file",
      descriptionOpt = Some("""If the given data is a sequence, write a line to the file for each item.
Otherwise, write the item as a string."""))
  }
  import Params._

  val params = ParameterModel(Seq(Append, File, Data))

  def apply(arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
    val append = boundParams(Append).isTruthy
    val file = boundParams.validatePath(File).toFile
    val data = boundParams(Data)
    data match {
      case xs: MashList ⇒
        val lines = xs.elements.map(ToStringifier.stringify)
        FileUtils.writeLines(file, lines.asJava, append)
      case x ⇒
        FileUtils.write(file, ToStringifier.stringify(x), StandardCharsets.UTF_8, append)
    }
    MashUnit
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    Seq(CompletionSpec.File)

  override def summary = "Write an object or sequence of objects to a file as a string"

  override def descriptionOpt = Some("""The default encoding is used to convert the strings to bytes. 
If multiple lines are written, the default line separator is used.""")

}