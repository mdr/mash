package com.github.mdr.mash.ns.http

import java.net.URI

import com.github.mdr.mash.evaluator.{ Arguments, ToStringifier }
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.json.AsJsonFunction
import com.github.mdr.mash.runtime._
import org.apache.http.HttpEntityEnclosingRequest
import org.apache.http.client.methods.HttpPost
import org.apache.http.client.params.{ ClientPNames, CookiePolicy }
import org.apache.http.entity.{ ContentType, StringEntity }
import org.apache.http.impl.client.{ BasicCookieStore, HttpClientBuilder }

object PostFunction extends MashFunction("http.post") {

  object Params {
    val Url = Parameter(
      name = "url",
      summary = "URL to send request to")
    val Body = Parameter(
      name = "body",
      summary = "Body of request")
    val BasicAuth = Parameter(
      name = "basicAuth",
      summary = "Basic authentication",
      descriptionOpt = Some("Must either be a String of the form 'username:password', or an object of the form { username: 'username', password: 'password' }"),
      defaultValueGeneratorOpt = Some(() ⇒ MashNull),
      isFlag=true)
    val Headers = Parameter(
      name = "headers",
      summary = "Headers to add to request",
      descriptionOpt = Some(
        """Headers can be provided either as an object or a list. Examples:
--headers={ header1: value }
--headers=["header1:value", "header2:value"]
--headers=[{ name: "header1", value: "value"}]"""),
      isFlag=true,
      defaultValueGeneratorOpt = Some(() => MashNull))
    val Json = Parameter(
      name = "json",
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
    val client = HttpClientBuilder.create.setDefaultCookieStore(cookieStore).build
    //    client.getParams.setParameter(ClientPNames.COOKIE_POLICY, CookiePolicy.BROWSER_COMPATIBILITY)

    setBody(request, bodyValue, json)

    val response = client.execute(request)

    GetFunction.asMashObject(response, cookieStore)
  }

  private def setBody(request: HttpEntityEnclosingRequest, bodyValue: MashValue, json: Boolean) {
    val bodyString = if (json) AsJsonFunction.asJsonString(bodyValue) else ToStringifier.stringify(bodyValue)
    val entity = new StringEntity(bodyString, ContentType.APPLICATION_JSON)
    request.setEntity(entity)
  }
  
  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(ResponseClass)

  override def summary = "Make an HTTP POST request"

}