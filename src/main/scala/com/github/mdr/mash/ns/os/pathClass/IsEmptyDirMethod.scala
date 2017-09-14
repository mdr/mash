package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.Files

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashBoolean, MashValue }

object IsEmptyDirMethod extends MashMethod("isEmptyDir") {

  private val fileSystem = LinuxFileSystem

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashBoolean = {
    val path = interpretAsPath(target)
    MashBoolean(Files.isDirectory(path) && fileSystem.getChildren(path, ignoreDotFiles = false, recursive = false).isEmpty)
  }

  override def typeInferenceStrategy = Boolean

  override def summaryOpt = Some("Check if path is an empty directory")

}
