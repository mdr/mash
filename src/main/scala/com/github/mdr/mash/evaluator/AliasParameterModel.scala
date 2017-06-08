package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions.{ Parameter, ParameterModel }

object AliasParameterModel {

  object Params {
    val Name = Parameter(Some("name"))
  }
  import Params._
  val params = ParameterModel(Name)

}
