package com.github.mdr.mash.ns.json

import java.net.URI

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.ns.http.ResponseClass.Wrapper
import com.github.mdr.mash.ns.http.{ BasicCredentials, Header, HttpFunctions, HttpOperations }
import com.github.mdr.mash.ns.json.FromFileFunction.parseJson
import com.github.mdr.mash.runtime._
import org.apache.http.client.methods.{ HttpPost, HttpPut }

object PutFunction extends MashFunction("json.put") {
  import HttpFunctions.Params._

  val params = ParameterModel(Seq(Url, Body, BasicAuth, Headers))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val headers = Header.getHeaders(boundParams, Headers)

    val url = new URI(boundParams.validateString(Url).s)
    val bodyValue = boundParams(Body)
    val basicCredentialsOpt = BasicCredentials.getBasicCredentials(boundParams, BasicAuth)
    val result = HttpOperations.runRequest(new HttpPut(url), headers, basicCredentialsOpt, Some(bodyValue), json = true)
    parseJson(Wrapper(result).body)
  }

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt = Some("PUT some JSON to an HTTP endpoint, and parse the result as JSON")

}