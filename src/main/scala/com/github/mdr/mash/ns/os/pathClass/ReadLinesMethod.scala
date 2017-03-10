package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.ReadLinesFunction
import com.github.mdr.mash.runtime.{ MashList, MashValue }

object ReadLinesMethod extends MashMethod("readLines") {

  val params = ParameterModel()

  def apply(target: MashValue, boundParams: BoundParams): MashList = {
    val path = interpretAsPath(target)
    ReadLinesFunction.readLines(path)
  }

  override def typeInferenceStrategy = Type.Seq(StringClass)

  override def summaryOpt = Some("Read lines from this file")

  override def descriptionOpt = Some(
    """Returns a sequence of lines read from this file.
The default character encoding and line separator are used.""")

}
