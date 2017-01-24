package com.github.mdr.mash.ns.http

import java.net.URI

import com.github.mdr.mash.evaluator.{ Arguments, ToStringifier }
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.json.PrettyPrintFunction
import com.github.mdr.mash.runtime._
import org.apache.http.HttpEntityEnclosingRequest
import org.apache.http.client.config.{ CookieSpecs, RequestConfig }
import org.apache.http.client.methods.HttpPost
import org.apache.http.entity.{ ContentType, StringEntity }
import org.apache.http.impl.client.{ BasicCookieStore, HttpClients }

object PostFunction extends MashFunction("http.post") {

  object Params {
    val Url = Parameter(
      nameOpt = Some("url"),
      summary = "URL to send request to")
    val Body = Parameter(
      nameOpt = Some("body"),
      summary = "Body of request",
      defaultValueGeneratorOpt = Some(() ⇒ MashString("")))
    val BasicAuth = Parameter(
      nameOpt = Some("basicAuth"),
      summary = "Basic authentication",
      descriptionOpt = Some("Must either be a String of the form 'username:password', or an object of the form { username: 'username', password: 'password' }"),
      defaultValueGeneratorOpt = Some(() ⇒ MashNull),
      isFlag=true)
    val Headers = Parameter(
      nameOpt = Some("headers"),
      summary = "Headers to add to request",
      descriptionOpt = Some(
        """Headers can be provided either as an object or a list. Examples:
--headers={ header1: value }
--headers=["header1:value", "header2:value"]
--headers=[{ name: "header1", value: "value"}]"""),
      isFlag=true,
      defaultValueGeneratorOpt = Some(() ⇒ MashNull))
    val Json = Parameter(
      nameOpt = Some("json"),
      summary = "Whether to send the body as JSON (default false)",
      shortFlagOpt = Some('j'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isBooleanFlag = true)
  }
  import Params._

  val params = ParameterModel(Seq(Url, Body, BasicAuth, Headers, Json))

  def apply(arguments: Arguments): MashObject = {
    val boundParams = params.validate(arguments)
    val headers = Header.getHeaders(boundParams, Headers)

    val url = new URI(boundParams.validateString(Url).s)
    val bodyValue = boundParams(Body)
    val json = boundParams(Json).isTruthy

    val request = new HttpPost(url)
    for (header <- headers)
      request.setHeader(header.name, header.value)
    BasicCredentials.getBasicCredentials(boundParams, BasicAuth).foreach(_.addCredentials(request))
    val cookieStore = new BasicCookieStore
    val client = HttpClients.custom()
      .setDefaultRequestConfig(RequestConfig.custom.setCookieSpec(CookieSpecs.DEFAULT).build())
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
  
  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(ResponseClass)

  override def summaryOpt = Some("Make an HTTP POST request")

}