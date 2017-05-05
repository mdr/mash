package com.github.mdr.mash.ns.core.stringClass

import java.time.Instant

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.runtime.{ MashNull, MashString, MashValue, MashWrapped }
import com.joestelmach.natty.Parser

import scala.collection.JavaConverters._

object ToDateTimeMethod extends MashMethod("toDateTime") {

  val params = ParameterModel()

  private val parser = new Parser

  def parseInstant(s: String): Option[Instant] =
    parser.parse(s).asScala.headOption.flatMap(_.getDates.asScala.headOption).map(_.toInstant)

  def call(target: MashValue, boundParams: BoundParams): MashValue = {
    parseInstant(target.asInstanceOf[MashString].s).map(MashWrapped).getOrElse(MashNull)
  }

  override def summaryOpt = Some("Parse this string as a DateTime")

  override def typeInferenceStrategy = DateTimeClass

}
