package com.github.mdr.mash.ns.view

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.MashObject

object PrintTreeFunction extends MashFunction("view.printTree") {

  object Params {
    val Data = Parameter(
      nameOpt = Some("data"),
      summaryOpt = Some("Data to view"))
  }
  import Params._

  val params = ParameterModel(Data)

  def call(boundParams: BoundParams): MashObject = {
    val data = boundParams(Data)
    ViewClass.build(data, printTree = true)
  }

  override def typeInferenceStrategy = ViewClass

  override def summaryOpt = Some("View data in the object browser, where possible")

}
