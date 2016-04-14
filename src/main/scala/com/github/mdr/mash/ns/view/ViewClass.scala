package com.github.mdr.mash.ns.view

import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.BooleanClass

object ViewClass extends MashClass("view.View") {

  object Fields {
    val Data = Field("data", "Data to display", Type.Any)
    val DisableCustomViews = Field("disableCustomViews", "If true, disable custom views for this data", Type.Instance(BooleanClass))
    val UseBrowser = Field("useBrowser", "If true, always use the object browser where possible", Type.Instance(BooleanClass))
  }

  import Fields._

  override lazy val fields = Seq(Data, DisableCustomViews, UseBrowser)

  def summary = "Instructions on how to display data in Mash's output system"
}