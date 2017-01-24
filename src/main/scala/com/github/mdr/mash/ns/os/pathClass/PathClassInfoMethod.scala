package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ FunctionHelpers, MashMethod, ParameterModel }
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type.classToType
import com.github.mdr.mash.ns.os.PathSummaryClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

object PathClassInfoMethod extends MashMethod("info") {

  private val fileSystem = LinuxFileSystem

  val params = ParameterModel()

  def apply(target: MashValue, arguments: Arguments): MashObject = {
    params.validate(arguments)
    val path = FunctionHelpers.interpretAsPath(target)
    val summary = fileSystem.getPathSummary(path)
    PathSummaryClass.asMashObject(summary)
  }

  override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(PathSummaryClass)

  override def summaryOpt = Some("Get PathSummary object for this path")

}