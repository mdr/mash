package com.github.mdr.mash.ns.json

import java.net.URI

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.ns.http.ResponseClass.Wrapper
import com.github.mdr.mash.ns.http._
import com.github.mdr.mash.runtime._
import org.apache.http.client.methods.HttpPost
import com.github.mdr.mash.ns.json.FromFileFunction.parseJson

object PostFunction extends MashFunction("json.post") {
  import HttpFunctions.Params._

  val params = ParameterModel(Seq(Url, Body, BasicAuth, Headers))

  def apply(boundParams: BoundParams): MashValue = {
    val headers = Header.getHeaders(boundParams, Headers)

    val url = new URI(boundParams.validateString(Url).s)
    val bodyValue = boundParams(Body)
    val basicCredentialsOpt = BasicCredentials.getBasicCredentials(boundParams, BasicAuth)
    val bodySource = BodySource.Value(bodyValue)
    val result = HttpOperations.runRequest(new HttpPost(url), headers, basicCredentialsOpt, Some(bodySource), json = true)
    parseJson(Wrapper(result).body)
  }

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt = Some("POST some JSON to an HTTP endpoint, and parse the result as JSON")

}