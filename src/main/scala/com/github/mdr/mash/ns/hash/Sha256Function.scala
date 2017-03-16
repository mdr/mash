package com.github.mdr.mash.ns.hash

import java.io.InputStream
import java.nio.file.Path

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.FunctionHelpers.safeInterpretAsPath
import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashString
import org.apache.commons.codec.digest.DigestUtils.sha256Hex
import org.apache.commons.io.FileUtils.openInputStream

object Sha256Function extends MashFunction("hash.sha256") {

  object Params {
    val Data = Parameter(
      nameOpt = Some("data"),
      summaryOpt = Some("Data to hash"),
      descriptionOpt = Some(
        """If a Path or PathSummary, then the contents of the file will be hashed.
          |Else, the given data will be turned into a String and hashed.""".stripMargin))
  }

  import Params._

  val params = ParameterModel(Seq(Data))

  def apply(boundParams: BoundParams): MashString = {
    val data = boundParams(Data)
    val s = safeInterpretAsPath(data, stringsMustHaveTags = true) match {
      case Some(path) ⇒ withFileInputStream(path)(sha256Hex)
      case None       ⇒ sha256Hex(ToStringifier.stringify(boundParams(Data)))
    }
    MashString(s)
  }

  override def typeInferenceStrategy = StringClass

  override def summaryOpt = Some("Calculate the SHA-256 hash of the given input data")

  private def withFileInputStream[T](path: Path)(f: InputStream ⇒ T) = {
    val inputStream = openInputStream(path.toFile)
    try
      f(inputStream)
    finally
      inputStream.close()
  }

}