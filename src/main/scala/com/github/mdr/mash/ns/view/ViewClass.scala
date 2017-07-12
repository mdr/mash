package com.github.mdr.mash.ns.view

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.runtime.{ MashBoolean, MashObject, MashValue }

import scala.collection.immutable.ListMap

object ViewClass extends MashClass("view.View") {

  def unpackView(value: MashValue): MashValue = value match {
    case obj@MashObject(_, Some(ViewClass)) ⇒ obj.get(ViewClass.Fields.Data).map(unpackView).getOrElse(obj)
    case result                             ⇒ value
  }

  object Fields {
    val Data = Field("data", Some("Data to display"), Type.Any)
    val DisableCustomViews = Field("disableCustomViews", Some("If true, disable custom views for this data"), BooleanClass)
    val UseBrowser = Field("useBrowser", Some("If true, always use the object browser where possible"), BooleanClass)
    val UseTree = Field("useTree", Some("If true, always use the tree object browser where possible"), BooleanClass)
  }

  import Fields._

  override lazy val fields = Seq(Data, DisableCustomViews, UseBrowser, UseTree)

  case class Wrapper(x: MashValue) extends AbstractObjectWrapper(x) {
    def disableCustomViews: Boolean = getBooleanField(DisableCustomViews)

    def useBrowser: Boolean = getBooleanField(UseBrowser)

    def useTree: Boolean = getBooleanField(UseTree)

    def data = getField(Data)
  }

  def build(data: MashValue,
            disableCustomViews: Boolean = false,
            useBrowser: Boolean = false,
            useTree: Boolean = false) =
    MashObject.of(ListMap(
      Data -> data,
      DisableCustomViews -> MashBoolean(disableCustomViews),
      UseBrowser -> MashBoolean(useBrowser),
      UseTree -> MashBoolean(useTree)), ViewClass)


  override val staticMethods = Seq(NewStaticMethod(this))

  override def summaryOpt = Some("Instructions on how to display data in Mash's output system")
}