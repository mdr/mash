package com.github.mdr.mash.ns.os

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime.{ MashBoolean, MashList, MashUnit, MashValue }
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._

object WriteFunction extends MashFunction("os.write") {

  object Params {
    val Append = Parameter(
      nameOpt = Some("append"),
      summaryOpt = Some("Append to the end of the file, if it already exists"),
      shortFlagOpt = Some('a'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isBooleanFlag = true)
    val File = Parameter(
      nameOpt = Some("file"),
      summaryOpt = Some("File to write to"))
    val Data = Parameter(
      nameOpt = Some("data"),
      summaryOpt = Some("Data to write to the file"),
      descriptionOpt = Some("""If the given data is a sequence, write a line to the file for each item.
Otherwise, write the item as a string."""))
  }
  import Params._

  val params = ParameterModel(Seq(Append, File, Data))

  def apply(boundParams: BoundParams): MashUnit = {
    val append = boundParams(Append).isTruthy
    val path = boundParams.validatePath(File)
    val data = boundParams(Data)
    write(path, data, append)
    MashUnit
  }

  def write(path: Path, data: MashValue, append: Boolean): Unit = {
    val file = path.toFile
    data match {
      case xs: MashList ⇒
        val lines = xs.elements.map(ToStringifier.stringify)
        FileUtils.writeLines(file, lines.asJava, append)
      case x            ⇒
        FileUtils.write(file, ToStringifier.stringify(x), StandardCharsets.UTF_8, append)
    }
  }

  override def typeInferenceStrategy = UnitClass

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    Seq(CompletionSpec.File)

  override def summaryOpt = Some("Write an object or sequence of objects to a file as a string")

  override def descriptionOpt = Some("""The default encoding is used to convert the strings to bytes. 
If multiple lines are written, the default line separator is used.""")

}