package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashValue, MashWrapped }

object LastModifiedMethod extends MashMethod("lastModified") {

  private val fileSystem = LinuxFileSystem

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashWrapped = {
    MashWrapped(fileSystem.getPathSummary(interpretAsPath(target)).lastModified)
  }

  override def typeInferenceStrategy = DateTimeClass

  override def summaryOpt = Some("Last time path was modified")

}
