package com.github.mdr.mash.ns.csv

import java.io.FileWriter
import java.nio.file.Path

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime.{ MashObject, MashUnit, MashValue }
import org.apache.commons.csv.{ CSVFormat, CSVPrinter }

object WriteFunction extends MashFunction("csv.write") {

  object Params {
    val File = Parameter(
      nameOpt = Some("file"),
      summaryOpt = Some("File to write CSV records to"))
    val Values = Parameter(
      nameOpt = Some("values"),
      summaryOpt = Some("Data to write"))
  }

  import Params._

  val params = ParameterModel(File, Values)

  def call(boundParams: BoundParams): MashUnit = {
    val path = boundParams.validatePath(File)
    val values = boundParams.validateSequence(Values, allowStrings = false)

    val fields =
      values.flatMap {
        case obj: MashObject ⇒ obj.immutableFields.keys.toSeq
        case x               ⇒ boundParams.throwInvalidArgument(Values, s"Values to write must be objects, but found: ${x.typeName}")
      }.distinct

    writeCsv(path, fields, values)
    MashUnit
  }

  private def writeCsv(path: Path, fields: Seq[String], values: Seq[MashValue]) {
    val csvFileFormat = CSVFormat.DEFAULT.withHeader(fields: _*)
    val fileWriter = new FileWriter(path.toFile)
    val csvFilePrinter = new CSVPrinter(fileWriter, csvFileFormat)

    for (value ← values) {
      val objOpt = PartialFunction.condOpt(value) { case obj: MashObject ⇒ obj }
      for (field ← fields) {
        val fieldValue = objOpt.flatMap(_ get field)
        val stringValue = fieldValue.map(ToStringifier.stringify) getOrElse ""
        csvFilePrinter.print(stringValue)
      }
      csvFilePrinter.println()

    }
    csvFilePrinter.close()
  }

  override def typeInferenceStrategy = UnitClass

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summaryOpt = Some("Write the given values as a CSV file")

}