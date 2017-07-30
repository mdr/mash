package com.github.mdr.mash.ns.core.stringClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.time.{ DateClass, DateTimeClass }
import com.github.mdr.mash.runtime.{ MashNull, MashString, MashValue, MashWrapped }

object ToDateMethod extends MashMethod("toDate") {

  val params = ParameterModel.Empty

  def call(target: MashValue, boundParams: BoundParams): MashValue =
    ToDateTimeMethod.parseInstant(target.asInstanceOf[MashString].s)
      .map(instant ⇒ DateTimeClass.Wrapper(MashWrapped(instant)).localDate)
      .map(MashWrapped)
      .getOrElse(MashNull)

  override def summaryOpt = Some("Parse this string as a Date")

  override def typeInferenceStrategy = DateClass

}
