package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.UsernameClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object OwnerMethod extends MashMethod("owner") {

  private val fileSystem = LinuxFileSystem

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    val summary = fileSystem.getPathSummary(interpretAsPath(target))
    MashString(summary.owner, Some(UsernameClass))
  }

  override def typeInferenceStrategy = StringClass taggedWith UsernameClass

  override def summaryOpt = Some("Owner of this path")

}
