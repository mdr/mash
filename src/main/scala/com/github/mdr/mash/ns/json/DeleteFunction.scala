package com.github.mdr.mash.ns.json

import java.net.URI

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.ns.http.ResponseClass.Wrapper
import com.github.mdr.mash.ns.http.{ BasicCredentials, Header, HttpFunctions, HttpOperations }
import com.github.mdr.mash.ns.json.FromFileFunction.parseJson
import com.github.mdr.mash.runtime._
import org.apache.http.client.methods.HttpDelete

object DeleteFunction extends MashFunction("json.delete") {
  import HttpFunctions.Params._

  val params = ParameterModel(Seq(Url, BasicAuth, Headers))

  def apply(boundParams: BoundParams): MashValue = {
    val headers = Header.getHeaders(boundParams, Headers)
    val url = new URI(boundParams.validateString(Url).s)
    val basicCredentialsOpt = BasicCredentials.getBasicCredentials(boundParams, BasicAuth)
    val result = HttpOperations.runRequest(new HttpDelete(url), headers, basicCredentialsOpt)
    parseJson(Wrapper(result).body)
  }

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt = Some("Make an HTTP DELETE request and return the response as JSON")

}