package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.{ Files, NoSuchFileException, NotLinkException }

import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, FunctionHelpers, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.runtime.{ MashString, MashValue }

object FollowLinkMethod extends MashMethod("followLink") {

  val params = ParameterModel()

  def call(target: MashValue, boundParams: BoundParams): MashString = {
    val path = FunctionHelpers.interpretAsPath(target)
    val resolved =
      try Files.readSymbolicLink(path)
      catch  {
        case _: NotLinkException ⇒ throw new EvaluatorException(s"Path '$path' is not a link")
        case _: NoSuchFileException ⇒ throw new EvaluatorException(s"Path '$path' does not exist")
      }
    MashString(resolved.toString, PathClass)
  }

  override def typeInferenceStrategy = StringClass taggedWith PathClass

  override def summaryOpt = Some("Follow this symbolic link")

}