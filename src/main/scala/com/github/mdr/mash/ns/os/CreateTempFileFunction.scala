package com.github.mdr.mash.ns.os

import java.nio.file.Files

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashString

object CreateTempFileFunction extends MashFunction("os.createTempFile") {

  val params = ParameterModel.Empty

  def call(boundParams: BoundParams): MashString = {
    val path = Files.createTempFile(null, null)
    asPathString(path)
  }

  override def typeInferenceStrategy = StringClass taggedWith PathClass

  override def summaryOpt = Some("Create a new temporary file")

}