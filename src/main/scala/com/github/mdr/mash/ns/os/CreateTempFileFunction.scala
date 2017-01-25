package com.github.mdr.mash.ns.os

import java.nio.file.Files

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ MashFunction, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashString

object CreateTempFileFunction extends MashFunction("os.createTempFile") {

  val params = ParameterModel(Seq())

  def apply(arguments: Arguments): MashString = {
    params.validate(arguments)
    val path = Files.createTempFile(null, null)
    asPathString(path)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(StringClass taggedWith PathClass)

  override def summaryOpt = Some("Create a new temporary file")

}