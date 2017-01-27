package com.github.mdr.mash.ns.http

import java.net.URI

import com.github.mdr.mash.evaluator.{ Arguments, ToStringifier }
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.json.PrettyPrintFunction
import com.github.mdr.mash.runtime._
import org.apache.http.HttpEntityEnclosingRequest
import org.apache.http.client.methods.{ HttpPost, HttpPut }
import org.apache.http.entity.{ ContentType, StringEntity }
import org.apache.http.impl.client.{ BasicCookieStore, HttpClientBuilder }

object PutFunction extends MashFunction("http.put") {

  object Params {
    val Url = PostFunction.Params.Url
    val BasicAuth = PostFunction.Params.BasicAuth
    val Headers = PostFunction.Params.Headers
    val Body = PostFunction.Params.Body
    val Json = PostFunction.Params.Json
  }
  import Params._

  val params = ParameterModel(Seq(Url, Body, BasicAuth, Headers, Json))

  def apply(arguments: Arguments): MashObject = {
    val boundParams = params.validate(arguments)
    val headers = Header.getHeaders(boundParams, Headers)

    val url = new URI(boundParams.validateString(Url).s)
    val bodyValue = boundParams(Body)
    val json = boundParams(Json).isTruthy

    val request = new HttpPut(url)
    for (header <- headers)
      request.setHeader(header.name, header.value)
    BasicCredentials.getBasicCredentials(boundParams, BasicAuth).foreach(_.addCredentials(request))
    val cookieStore = new BasicCookieStore
    val client = HttpClientBuilder.create
      .setDefaultCookieStore(cookieStore)
      .setSSLContext(InsecureSsl.makeInsecureSslContext())
      .setSSLHostnameVerifier(InsecureSsl.TrustAllHostnameVerifier)
      .build
    setBody(request, bodyValue, json)

    val response = client.execute(request)

    GetFunction.asMashObject(response, cookieStore)
  }

  private def setBody(request: HttpEntityEnclosingRequest, bodyValue: MashValue, json: Boolean) {
    val bodyString = if (json) PrettyPrintFunction.asJsonString(bodyValue) else ToStringifier.stringify(bodyValue)
    val entity = new StringEntity(bodyString, ContentType.APPLICATION_JSON)
    request.setEntity(entity)
  }
  
  override def typeInferenceStrategy = ResponseClass

  override def summaryOpt = Some("Make an HTTP PUT request")

}