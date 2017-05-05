package com.github.mdr.mash.ns.http

import java.net.URI

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.runtime._
import org.apache.http.client.methods.HttpPost

object PostFunction extends MashFunction("http.post") {

  import HttpFunctions.Params._

  val params = ParameterModel(Seq(Url, Body, File, Json, Form, BasicAuth, Headers, QueryParams))

  def call(boundParams: BoundParams): MashObject = {
    val headers = Header.getHeaders(boundParams, Headers)
    val url = QueryParameters.getUrl(boundParams)
    val bodySource = HttpFunctions.getBodySource(boundParams)
    val json = boundParams(Json).isTruthy
    val form = boundParams(Form).isTruthy
    if (json && form)
      boundParams.throwInvalidArgument(Json, s"Incompatible arguments ${Json.name} and ${Form.name}")
    if (form)
      bodySource match {
        case BodySource.Value(obj: MashObject) ⇒
        case _                                 ⇒
          boundParams.throwInvalidArgument(Form, "Invalid body, must be an object")
      }
    val basicCredentialsOpt = BasicCredentials.getBasicCredentials(boundParams, BasicAuth)
    HttpOperations.runRequest(new HttpPost(url), headers, basicCredentialsOpt, Some(bodySource), json, form)
  }


  override def typeInferenceStrategy = ResponseClass

  override def summaryOpt = Some("Make an HTTP POST request")

}