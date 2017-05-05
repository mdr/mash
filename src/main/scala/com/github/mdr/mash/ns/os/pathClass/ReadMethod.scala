package com.github.mdr.mash.ns.os.pathClass

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object ReadMethod extends MashMethod("read") {
  private val fileSystem = LinuxFileSystem

  val params = ParameterModel()

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    val path = interpretAsPath(target)
    MashString(fileSystem.read(path))
  }

  override def typeInferenceStrategy = StringClass

  override def summaryOpt = Some("Read the contents of this file as a string")

  override def descriptionOpt = Some(
    """Returns a string with the contents of this file.
The default character encoding is used.""")

}
