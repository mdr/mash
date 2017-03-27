package com.github.mdr.mash.ns.json

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.ns.http.ResponseClass.Wrapper
import com.github.mdr.mash.ns.http._
import com.github.mdr.mash.ns.json.FromFileFunction.parseJson
import com.github.mdr.mash.runtime._
import org.apache.http.client.methods.HttpPost

object PostFunction extends MashFunction("json.post") {
  import HttpFunctions.Params._

  val params = ParameterModel(Seq(Url, Body, File, BasicAuth, Headers, QueryParams))

  def apply(boundParams: BoundParams): MashValue = {
    val headers = Header.getHeaders(boundParams, Headers)
    val url = QueryParameters.getUrl(boundParams)
    val bodySource = HttpFunctions.getBodySource(boundParams)
    val basicCredentialsOpt = BasicCredentials.getBasicCredentials(boundParams, BasicAuth)
    val result = HttpOperations.runRequest(new HttpPost(url), headers, basicCredentialsOpt, Some(bodySource), json = true)
    parseJson(Wrapper(result).body)
  }

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt = Some("POST some JSON to an HTTP endpoint, and parse the result as JSON")

}