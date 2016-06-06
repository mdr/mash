package com.github.mdr.mash.ns.os

import scala.collection.JavaConverters._
import com.github.mdr.mash.Posix
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.MashString

object PwdFunction extends MashFunction("os.pwd") {

  private val fileSystem = LinuxFileSystem

  val params = ParameterModel()

  def apply(arguments: Arguments): MashString = {
    params.validate(arguments)
    asPathString(fileSystem.pwd)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Tagged(StringClass, PathClass))

  override def summary = "Return the current working directory"

}