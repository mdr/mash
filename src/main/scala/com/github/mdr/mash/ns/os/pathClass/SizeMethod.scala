package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.{ BytesClass, NumberClass }
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashNumber, MashValue }

object SizeMethod extends MashMethod("size") {

  private val fileSystem = LinuxFileSystem

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashNumber = {
    val summary = fileSystem.getPathSummary(interpretAsPath(target))
    MashNumber(summary.size, Some(BytesClass))
  }

  override def typeInferenceStrategy = NumberClass taggedWith BytesClass

  override def summaryOpt = Some("Size of the file at this path")

}
