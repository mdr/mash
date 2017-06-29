package com.github.mdr.mash.ns.http

import com.github.mdr.mash.evaluator.ToStringifier.stringify
import com.github.mdr.mash.functions.{ BoundParams, Parameter }
import com.github.mdr.mash.ns.core.NoArgFunction
import com.github.mdr.mash.runtime.MashObject

object Cookie {

  def getCookies(boundParams: BoundParams, param: Parameter): Map[String, String] =
    NoArgFunction.option(boundParams(param)).toSeq.flatMap {
      case obj: MashObject ⇒
        obj.immutableFields.toSeq.map { case (fieldName, value) ⇒ stringify(fieldName) -> stringify(value) }
      case _               ⇒
        Seq()
    }.toMap

}
