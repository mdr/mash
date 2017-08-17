package com.github.mdr.mash.ns.os

import java.io.File
import java.nio.file.{ Files, Paths }

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.runtime.{ MashNull, MashValue }

object FindExecutableInPathFunction extends MashFunction("os.findExecutableInPath") {

  private val environmentInteractions = LinuxEnvironmentInteractions

  object Params {
    val Name = Parameter(
      nameOpt = Some("name"),
      summaryOpt = Some("Name of executable"))
  }

  import Params._

  val params = ParameterModel(Name)

  def call(boundParams: BoundParams): MashValue = {
    val name = boundParams.validateString(Name).s
    val segments = environmentInteractions.path.split(File.pathSeparator)
    segments.map(Paths.get(_, name)).find(Files.isExecutable).map(asPathString(_)).getOrElse(MashNull)
  }

  override def summaryOpt: Option[String] = Some("""Find full path to an executable on the path""")
}
