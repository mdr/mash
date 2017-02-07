package com.github.mdr.mash.ns.view

import com.github.mdr.mash.evaluator.{ Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.BooleanClass

object ViewClass extends MashClass("view.View") {

  object Fields {
    val Data = Field("data", Some("Data to display"), Type.Any)
    val DisableCustomViews = Field("disableCustomViews", Some("If true, disable custom views for this data"), BooleanClass)
    val UseBrowser = Field("useBrowser", Some("If true, always use the object browser where possible"), BooleanClass)
    val UseTree = Field("useTree", Some("If true, always use the tree object browser where possible"), BooleanClass)
  }

  import Fields._

  override lazy val fields = Seq(Data, DisableCustomViews, UseBrowser, UseTree)

  override val staticMethods = Seq(NewStaticMethod(this))

  override def summaryOpt = Some("Instructions on how to display data in Mash's output system")
}