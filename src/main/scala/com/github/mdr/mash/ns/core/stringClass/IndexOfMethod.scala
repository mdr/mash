package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.runtime.{ MashNull, MashNumber, MashString, MashValue }

object IndexOfMethod extends MashMethod("indexOf") {

  object Params {
    val Substring = Parameter(
      nameOpt = Some("substring"),
      summaryOpt = Some("Substring to find"))
  }

  import Params._

  val params = ParameterModel(Substring)

  def call(target: MashValue, boundParams: BoundParams): MashValue = {
    val s = target.asInstanceOf[MashString].s
    val substring = boundParams.validateString(Substring).s
    s.indexOf(substring) match {
      case -1 ⇒ MashNull
      case n  ⇒ MashNumber(n)
    }
  }

  override def typeInferenceStrategy = NumberClass

  override def summaryOpt = Some("Return the index of the first occurrence of the given substring, if any, else null")

  override def descriptionOpt = Some(
    """Examples:
  indexOf 2 [1, 2, 3] # 1
  indexOf 9 [1, 2, 3] # null""")


}

