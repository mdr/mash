package com.github.mdr.mash.ns.view

import java.time._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.functions.Parameter
import scala.collection.immutable.ListMap
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashBoolean

object RawFunction extends MashFunction("view.raw") {

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
      DisableCustomViews -> MashBoolean.True,
      UseBrowser -> MashBoolean.False), ViewClass)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(ViewClass)

  override def summary = "View an object without custom formatting"

}
