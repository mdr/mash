package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.os.PermissionsClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

object PermissionsMethod extends MashMethod("permissions") {

  private val fileSystem = LinuxFileSystem

  val params = ParameterModel()

  def call(target: MashValue, boundParams: BoundParams): MashObject = {
    val summary = fileSystem.getPathSummary(interpretAsPath(target))
    val permissions = summary.permissions
    PermissionsClass.asMashObject(permissions)
  }

  override def typeInferenceStrategy = PermissionsClass

  override def summaryOpt = Some("Permissions for this path")

}

