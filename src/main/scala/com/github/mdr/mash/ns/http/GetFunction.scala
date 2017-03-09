package com.github.mdr.mash.ns.http

import java.net.URI

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.runtime._
import org.apache.http.client.methods.HttpGet

object GetFunction extends MashFunction("http.get") {

  import HttpFunctions.Params._

  val params = ParameterModel(Seq(Url, BasicAuth, Headers))

  def apply(boundParams: BoundParams): MashObject = {
    val headers = Header.getHeaders(boundParams, Headers)

    val url = new URI(boundParams.validateString(Url).s)
    val json = boundParams(Json).isTruthy
    val basicCredentialsOpt = BasicCredentials.getBasicCredentials(boundParams, BasicAuth)
    HttpOperations.runRequest(new HttpGet(url), headers, basicCredentialsOpt, json = json)
  }

  override def typeInferenceStrategy = ResponseClass

  override def summaryOpt = Some("Make an HTTP GET request")

}