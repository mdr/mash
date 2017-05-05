package com.github.mdr.mash.ns.json


import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.ns.http.ResponseClass.Wrapper
import com.github.mdr.mash.ns.http._
import com.github.mdr.mash.ns.json.FromFileFunction.parseJson
import com.github.mdr.mash.runtime._
import org.apache.http.client.methods.HttpGet

object GetFunction extends MashFunction("json.get") {
  import HttpFunctions.Params._

  val params = ParameterModel(Seq(Url, BasicAuth, Headers, QueryParams))

  def call(boundParams: BoundParams): MashValue = {
    val headers = Header.getHeaders(boundParams, Headers)
    val url = QueryParameters.getUrl(boundParams)
    val basicCredentialsOpt = BasicCredentials.getBasicCredentials(boundParams, BasicAuth)
    val result = HttpOperations.runRequest(new HttpGet(url), headers, basicCredentialsOpt, json = true)
    parseJson(Wrapper(result).body)
  }

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt = Some("Make an HTTP GET request and return the response as JSON")

}