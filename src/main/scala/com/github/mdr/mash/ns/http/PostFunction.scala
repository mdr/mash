package com.github.mdr.mash.ns.http

import java.net.URI

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.runtime._
import org.apache.http.client.methods.HttpPost

object PostFunction extends MashFunction("http.post") {

  import HttpFunctions.Params._

  val params = ParameterModel(Seq(Url, Body, File, Json, BasicAuth, Headers))

  def apply(boundParams: BoundParams): MashObject = {
    val headers = Header.getHeaders(boundParams, Headers)

    val url = new URI(boundParams.validateString(Url).s)
    val bodySource = HttpFunctions.getBodySource(boundParams)
    val json = boundParams(Json).isTruthy
    val basicCredentialsOpt = BasicCredentials.getBasicCredentials(boundParams, BasicAuth)
    HttpOperations.runRequest(new HttpPost(url), headers, basicCredentialsOpt, Some(bodySource), json)
  }

  override def typeInferenceStrategy = ResponseClass

  override def summaryOpt = Some("Make an HTTP POST request")

}