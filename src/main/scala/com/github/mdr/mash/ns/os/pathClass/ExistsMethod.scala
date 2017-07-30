package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime._

object ExistsMethod extends MashMethod("exists") {

  private val fileSystem = LinuxFileSystem

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashBoolean = {
    val path = FunctionHelpers.interpretAsPath(target)
    MashBoolean(fileSystem.exists(path))
  }

  override def typeInferenceStrategy = BooleanClass

  override def summaryOpt = Some("Whether or not an item exists at this location")

}