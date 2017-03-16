package com.github.mdr.mash.ns.view

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.{ MashBoolean, MashObject }

import scala.collection.immutable.ListMap

object RawFunction extends MashFunction("view.raw") {

  object Params {
    val Data = Parameter(
      nameOpt = Some("data"),
      summaryOpt = Some("Data to view"))
  }
  import Params._

  val params = ParameterModel(Seq(Data))

  def apply(boundParams: BoundParams): MashObject = {
    val data = boundParams(Data)
    import ViewClass.Fields._
    MashObject.of(ListMap(
      Data -> data,
      DisableCustomViews -> MashBoolean.True,
      UseBrowser -> MashBoolean.False,
      UseTree -> MashBoolean.False), ViewClass)
  }

  override def typeInferenceStrategy = ViewClass

  override def summaryOpt = Some("View an object without custom formatting")

}
