package com.github.mdr.mash.ns.core

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

object NoArgFunction extends MashFunction("core.noArg") {

  val NoArgValue: MashValue = MashObject.empty.withClass(NoArgClass)

  def option(x: MashValue): Option[MashValue] = x match {
    case NoArgValue ⇒ None
    case _          ⇒ Some(x)
  }

  val params = ParameterModel()

  def call(boundParams: BoundParams): MashValue = NoArgValue

  override def summaryOpt = Some("Value that represents no argument being provided, for use as sentinel values in default arguments")

}
