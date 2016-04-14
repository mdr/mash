package com.github.mdr.mash.ns.view

import java.time._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.functions.Parameter
import scala.collection.immutable.ListMap

object BrowserFunction extends MashFunction("view.browser") {

  object Params {
    val Data = Parameter(
      name = "data",
      summary = "Data to view")
  }
  import Params._

  val params = ParameterModel(Seq(Data))

  def apply(arguments: Arguments): MashObject = {
    val boundParams = params.validate(arguments)
    val data = boundParams(Data)
    import ViewClass.Fields._
    MashObject(ListMap(
      Data -> data,
      DisableCustomViews -> false,
      UseBrowser -> true), ViewClass)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(ViewClass))

  override def summary = "View data in the object browser, where possible"

}
