package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ BoundParams, FunctionHelpers, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.{ MashString, MashValue }
import org.apache.commons.io.FilenameUtils

object BaseNameMethod extends MashMethod("baseName") {

  val params = ParameterModel()

  def apply(target: MashValue, boundParams: BoundParams): MashString = {
    val name = FunctionHelpers.interpretAsPath(target).getFileName.toString
    MashString(FilenameUtils.getBaseName(name))
  }

  override def typeInferenceStrategy = StringClass

  override def summaryOpt = Some("Name without extension")

}
