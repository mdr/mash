package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.Files

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.UnitClass
import com.github.mdr.mash.runtime.{ MashUnit, MashValue }
import org.apache.commons.io.FileUtils

object DeleteMethod extends MashMethod("delete") {

  override def aliases = Seq("rm")

  val params = ParameterModel()

  def apply(target: MashValue, boundParams: BoundParams): MashUnit = {
    val path = interpretAsPath(target)
    if (Files.isDirectory(path))
      FileUtils.deleteDirectory(path.toFile)
    else
      Files.delete(path)
    MashUnit
  }

  override def typeInferenceStrategy = UnitClass

  override def summaryOpt = Some("Delete this path")

}
