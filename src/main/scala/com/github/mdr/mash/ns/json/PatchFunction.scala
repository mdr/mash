package com.github.mdr.mash.ns.json

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.ns.http.ResponseClass.Wrapper
import com.github.mdr.mash.ns.http._
import com.github.mdr.mash.ns.json.ReadFunction.parseJson
import com.github.mdr.mash.runtime._
import org.apache.http.client.methods.HttpPatch

object PatchFunction extends MashFunction("json.patch") {
  import HttpFunctions.Params._

  val params = ParameterModel(Url, Body, File, BasicAuth, Headers, Cookies, QueryParams)

  def call(boundParams: BoundParams): MashValue = {
    val headers = Header.getHeaders(boundParams, Headers)
    val cookies = Cookie.getCookies(boundParams, Cookies)
    val url = QueryParameters.getUrl(boundParams)
    val bodySource = HttpFunctions.getBodySource(boundParams)
    val basicCredentialsOpt = BasicCredentials.getBasicCredentials(boundParams, BasicAuth)
    val result = HttpOperations.runRequest(new HttpPatch(url), headers, cookies, basicCredentialsOpt, Some(bodySource), json = true)
    parseJson(Wrapper(result).body)
  }

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt = Some("PATCH some JSON to an HTTP endpoint, and parse the result as JSON")

}