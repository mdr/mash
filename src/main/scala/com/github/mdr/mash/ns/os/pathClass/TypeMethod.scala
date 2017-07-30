package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.FileTypeClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object TypeMethod extends MashMethod("type") {

  private val fileSystem = LinuxFileSystem

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    val summary = fileSystem.getPathSummary(interpretAsPath(target))
    MashString(summary.fileType, Some(FileTypeClass))
  }

  override def typeInferenceStrategy = StringClass taggedWith FileTypeClass

  override def summaryOpt = Some("Type of object at this path (file, directory etc)")

}
