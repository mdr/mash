package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.Files

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ FunctionHelpers, MashMethod, ParameterModel }
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object PathClassFollowLinkMethod extends MashMethod("followLink") {

  val params = ParameterModel()

  def apply(target: MashValue, arguments: Arguments): MashString = {
    params.validate(arguments)
    val path = FunctionHelpers.interpretAsPath(target)
    val resolved = Files.readSymbolicLink(path)
    MashString(resolved.toString, PathClass)
  }

  override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass taggedWith PathClass)

  override def summary = "Follow this symbolic link"

}