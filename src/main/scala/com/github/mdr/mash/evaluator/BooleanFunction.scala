package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.{ MashUnit, MashValue }

class BooleanFunction(b: Boolean) extends MashFunction() {

  object Params {
    val Then = Parameter(
      nameOpt = Some("then"),
      summaryOpt = Some("The result if this is true"),
      isLazy = true)
    val Else = Parameter(
      nameOpt = Some("else"),
      summaryOpt = Some("The result if this is false"),
      defaultValueGeneratorOpt = Some(MashUnit),
      isLazy = true)
  }

  import Params._

  val params = ParameterModel(Then, Else)

  def call(boundParams: BoundParams): MashValue =
    if (b)
      boundParams(Then).asInstanceOf[MashFunction].callNullary()
    else
      boundParams(Else) match {
        case MashUnit        ⇒ MashUnit
        case f: MashFunction ⇒ f.callNullary()
      }

  override def summaryOpt = Some("Boolean as a function")

}
