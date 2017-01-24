package com.github.mdr.mash.ns.view

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.runtime.{ MashBoolean, MashObject }

import scala.collection.immutable.ListMap

object BrowserFunction extends MashFunction("view.browser") {

  object Params {
    val Data = Parameter(
      nameOpt = Some("data"),
      summaryOpt = Some("Data to view"))
  }
  import Params._

  val params = ParameterModel(Seq(Data))

  def apply(arguments: Arguments): MashObject = {
    val boundParams = params.validate(arguments)
    val data = boundParams(Data)
    import ViewClass.Fields._
    MashObject.of(ListMap(
      Data -> data,
      DisableCustomViews -> MashBoolean.False,
      UseBrowser -> MashBoolean.True,
      UseTree -> MashBoolean.False), ViewClass)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(ViewClass)

  override def summaryOpt = Some("View data in the object browser, where possible")

}
