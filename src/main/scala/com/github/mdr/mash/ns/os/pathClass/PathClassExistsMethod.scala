package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime._

object PathClassExistsMethod extends MashMethod("exists") {

  private val fileSystem = LinuxFileSystem

  val params = ParameterModel()

  def apply(target: MashValue, arguments: Arguments): MashBoolean = {
    params.bindTo(arguments)
    val path = FunctionHelpers.interpretAsPath(target)
    MashBoolean(fileSystem.exists(path))
  }

  override def typeInferenceStrategy = BooleanClass

  override def summaryOpt = Some("Whether or not an item exists at this location")

}