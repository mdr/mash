package com.github.mdr.mash.ns.view

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.{ MashBoolean, MashObject }

import scala.collection.immutable.ListMap

object TreeFunction extends MashFunction("view.tree") {

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
      DisableCustomViews -> MashBoolean.False,
      UseBrowser -> MashBoolean.False,
      UseTree -> MashBoolean.True), ViewClass)
  }

  override def typeInferenceStrategy = ViewClass

  override def summaryOpt = Some("View data in the object tree browser, where possible")

}
