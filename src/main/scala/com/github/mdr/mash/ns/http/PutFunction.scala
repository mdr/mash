package com.github.mdr.mash.ns.http

import java.net.URI

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.runtime._
import org.apache.http.client.methods.HttpPut

object PutFunction extends MashFunction("http.put") {
  import HttpFunctions.Params._

  val params = ParameterModel(Seq(Url, Body, BasicAuth, Headers, Json))

  def apply(boundParams: BoundParams): MashObject = {
    val headers = Header.getHeaders(boundParams, Headers)

    val url = new URI(boundParams.validateString(Url).s)
    val bodyValue = boundParams(Body)
    val json = boundParams(Json).isTruthy

    val basicCredentialsOpt = BasicCredentials.getBasicCredentials(boundParams, BasicAuth)
    HttpOperations.runRequest(new HttpPut(url), headers, basicCredentialsOpt, Some(bodyValue), json)
  }

  override def typeInferenceStrategy = ResponseClass

  override def summaryOpt = Some("Make an HTTP PUT request")

}