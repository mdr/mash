package com.github.mdr.mash.ns.view

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.MashObject

object PrintFunction extends MashFunction("view.print") {

  object Params {
    val Data = Parameter(
      nameOpt = Some("data"),
      summaryOpt = Some("Data to print"))
  }
  import Params._

  val params = ParameterModel(Data)

  def call(boundParams: BoundParams): MashObject = {
    val data = boundParams(Data)
    ViewClass.build(data, print = true)
  }

  override def typeInferenceStrategy = ViewClass

  override def summaryOpt = Some("Print the data")

}
