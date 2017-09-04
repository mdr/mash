package com.github.mdr.mash.view.render.help

import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashValue

object TestPointClass extends MashClass("geometry.Point") {

  val Name = Field("x", Some("The x coordinate"), descriptionOpt = Some(
    """The horizontal coordinate. Examples:
      |<mash>
      |  point.x
      |</mash>""".stripMargin), fieldType = StringClass)

  override def fields = Seq(Name)

  override def summaryOpt: Option[String] = Some("A point in 2D space")

  override def descriptionOpt = Some("Has an x and y coordinate")

  object TestRandomStaticMethod extends MashFunction("random") {

    override def call(boundParams: BoundParams): MashValue = ???

    override def summaryOpt: Option[String] = Some("Generate a random point")

    override def params: ParameterModel = ParameterModel.Empty

  }

  object TestNormMethod extends MashMethod("norm") {

    override def call(target: MashValue, boundParams: BoundParams): MashValue = ???

    override def summaryOpt: Option[String] = Some("Calculate the norm of the point")

    override def params: ParameterModel = ParameterModel.Empty

  }

  override def methods = Seq(TestNormMethod)

  override def staticMethods = Seq(TestRandomStaticMethod)
}
