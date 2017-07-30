package com.github.mdr.mash.ns.os

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.MashString

object CurrentDirectoryFunction extends MashFunction("os.currentDirectory") {

  private val fileSystem = LinuxFileSystem

  val params = ParameterModel.Empty

  def call(boundParams: BoundParams): MashString = {
    asPathString(fileSystem.pwd)
  }

  override def typeInferenceStrategy = StringClass taggedWith PathClass

  override def summaryOpt = Some("Return the current working directory")

}