package com.github.mdr.mash.ns.http

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, Parameter }
import com.github.mdr.mash.ns.core.NoArgFunction
import com.github.mdr.mash.runtime._

case class Header(name: String, value: String)

object Header {

  def getHeaders(boundParams: BoundParams, param: Parameter): Seq[Header] = {
    def getHeader(x: MashValue): Header = x match {
      case MashString(s, _) ⇒
        val chunks = s.split(":").toSeq
        if (chunks.length != 2)
          boundParams.throwInvalidArgument(param, "Header string must contain a single ':'")
        val Seq(name, value) = chunks
        Header(name, value)
      case obj: MashObject  ⇒
        def get(fieldName: String): String =
          ToStringifier.stringify(obj.get(fieldName).getOrElse(
            boundParams.throwInvalidArgument(param, s"Header object must have '$fieldName' field")))
        Header(get("name"), get("value"))
      case value            ⇒
        boundParams.throwInvalidArgument(param, "Invalid header of type " + value.typeName)
    }
    NoArgFunction.option(boundParams(param)).toSeq.flatMap {
      case xs: MashList ⇒
        xs.elements.map(getHeader)
      case obj: MashObject ⇒
        obj.fields.toSeq.map { case (fieldName, value) ⇒ Header(fieldName, ToStringifier.stringify(value)) }
      case _            ⇒
        Seq()
    }
  }

}