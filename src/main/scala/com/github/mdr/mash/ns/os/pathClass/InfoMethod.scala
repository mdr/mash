package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.functions.{ BoundParams, FunctionHelpers, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.os.PathSummaryClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

object InfoMethod extends MashMethod("info") {

  private val fileSystem = LinuxFileSystem

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashObject = {
    val path = FunctionHelpers.interpretAsPath(target)
    val summary = fileSystem.getPathSummary(path)
    PathSummaryClass.asMashObject(summary)
  }

  override def typeInferenceStrategy = PathSummaryClass

  override def summaryOpt = Some("Get PathSummary object for this path")

}