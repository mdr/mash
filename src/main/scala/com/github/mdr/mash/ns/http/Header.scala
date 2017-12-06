package com.github.mdr.mash.ns.http

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, Parameter }
import com.github.mdr.mash.ns.core.NoArgFunction
import com.github.mdr.mash.runtime._

case class Header(name: String, value: String)

object Header {

  def getHeaders(boundParams: BoundParams, param: Parameter): Seq[Header] = {

    NoArgFunction.option(boundParams(param)).toSeq.flatMap {
      case xs: MashList    ⇒
        xs.immutableElements.map(getHeaderFromItem(boundParams, param))
      case obj: MashObject ⇒
        getHeadersFromObject(obj)
      case _               ⇒
        Seq()
    }
  }

  private def getHeaderFromItem(boundParams: BoundParams, param: Parameter)(item: MashValue): Header =
    item match {
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

  private def getHeadersFromObject(obj: MashObject) =
    obj.immutableFields.toSeq.flatMap { case (fieldName, value) ⇒
      val headerName = ToStringifier.stringify(fieldName)
      val values = value match {
        case xs: MashList ⇒
          xs.immutableElements
        case _            ⇒
          Seq(value)
      }
      values.map(value ⇒ Header(headerName, ToStringifier.stringify(value)))
    }

}