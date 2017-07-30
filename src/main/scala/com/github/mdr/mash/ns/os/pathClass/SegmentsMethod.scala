package com.github.mdr.mash.ns.os.pathClass

import java.nio.file.Path

import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.{ MashList, MashValue }

import scala.collection.JavaConverters._

object SegmentsMethod extends MashMethod("segments") {

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashList = {
    val segments: Seq[Path] = interpretAsPath(target).asScala.toSeq
    MashList(segments.map(asPathString))
  }

  override def typeInferenceStrategy = Seq(StringClass)

  override def summaryOpt = Some("A sequence of the segments of this path (the parts of the path separated by /)")

}
