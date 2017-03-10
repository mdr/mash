package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.GroupClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object GroupMethod extends MashMethod("group") {

  private val fileSystem = LinuxFileSystem

  val params = ParameterModel()

  def apply(target: MashValue, boundParams: BoundParams): MashString = {
    val summary = fileSystem.getPathSummary(interpretAsPath(target))
    MashString(summary.group, Some(GroupClass))
  }

  override def typeInferenceStrategy = StringClass taggedWith GroupClass

  override def summaryOpt = Some("Group owner of this path")

}
