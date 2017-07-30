package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.os.PathSummaryClass
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.{ MashList, MashString, MashValue }

object GlobMethod extends MashMethod("glob") {

  private val fileSystem = LinuxFileSystem

  override def aliases = Seq("g")

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashList = {
    val pattern = target.asInstanceOf[MashString].s
    MashList(fileSystem.glob(pattern).map(PathSummaryClass.asMashObject))
  }

  override def typeInferenceStrategy = Seq(PathSummaryClass)

  override def summaryOpt = Some("Return paths matching a glob pattern")

}
