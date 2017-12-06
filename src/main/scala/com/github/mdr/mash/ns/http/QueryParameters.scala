package com.github.mdr.mash.ns.http

import java.net.URI

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.BoundParams
import com.github.mdr.mash.ns.http.HttpFunctions.Params._
import com.github.mdr.mash.runtime.{ MashList, MashValue }
import org.apache.http.client.utils.URIBuilder

object QueryParameters {

  def getUrl(boundParams: BoundParams): URI = {
    val url = new URI(boundParams.validateString(Url).s)
    val queryParams = getQueryParams(boundParams)
    addQueryParameters(url, queryParams)
  }

  private def getQueryParams(boundParams: BoundParams): Seq[(String, String)] = {
    val queryParamsObj = boundParams.validateObject(HttpFunctions.Params.QueryParams)
    for {
      (field, fieldValue) ← queryParamsObj.immutableFields.toSeq
      value ← getValues(fieldValue)
    } yield ToStringifier.stringify(field) -> ToStringifier.stringify(value)
  }

  private def getValues(value: MashValue): Seq[MashValue] = value match {
    case xs: MashList ⇒ xs.immutableElements
    case _            ⇒ Seq(value)
  }

  private def addQueryParameters(url: URI, queryParams: Seq[(String, String)]): URI = {
    val builder = new URIBuilder(url)
    for ((name, value) ← queryParams)
      builder.addParameter(name, value)
    builder.build
  }

}
