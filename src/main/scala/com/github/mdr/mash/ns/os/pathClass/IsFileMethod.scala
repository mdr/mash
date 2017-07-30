package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.Files

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.runtime.{ MashBoolean, MashValue }

object IsFileMethod extends MashMethod("isFile") {

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashBoolean = {
    MashBoolean(Files.isRegularFile(interpretAsPath(target)))
  }

  override def typeInferenceStrategy = BooleanClass

  override def summaryOpt = Some("Check if path is a directory")

}
