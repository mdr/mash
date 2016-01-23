package com.github.mdr.mash.ns.os

import java.nio.file.Files
import java.nio.file.Path
import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap
import org.apache.commons.io.FileUtils
import com.github.mdr.mash.Posix
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.BytesClass
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.FunctionHelpers
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.ns.core.UnitClass

object WriteFunction extends MashFunction("os.write") {

  object Params {
    val File = Parameter(
      name = "file",
      summary = "File to write to",
      isLast = true)
    val Data = Parameter(
      name = "data",
      summary = "Data to write to the file",
      descriptionOpt = Some("""If the given data is a sequence, write a line to the file for each item.
Otherwise, write the item as a string."""))
  }
  import Params._

  val params = ParameterModel(Seq(Data, File))

  def apply(arguments: Arguments) {
    val boundParams = params.validate(arguments)
    val file = boundParams.validatePath(File).toFile
    boundParams(Data) match {
      case xs: MashList ⇒
        FileUtils.writeLines(file, xs.items.map(x ⇒ ToStringifier.stringify(x)).asJava)
      case x ⇒
        FileUtils.write(file, ToStringifier.stringify(x))
    }
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UnitClass))

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    Seq(CompletionSpec.File)

  override def summary = "Write an object or sequence of objects to a file as a string"

  override def descriptionOpt = Some("""The default encoding is used to convert the strings to bytes. 
If multiple lines are written, the default line separator is used.""")

}