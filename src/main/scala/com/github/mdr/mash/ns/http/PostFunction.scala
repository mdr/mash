package com.github.mdr.mash.ns.http

import java.net.URI
import scala.collection.immutable.ListMap
import org.apache.commons.io.IOUtils
import org.apache.http.client.methods.HttpPost
import org.apache.http.entity.ContentType
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.HttpClientBuilder
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.http.ResponseClass.Fields
import com.github.mdr.mash.ns.json.AsJsonFunction
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.evaluator.ToStringifier
import org.apache.http.HttpRequest
import org.apache.http.HttpEntityEnclosingRequest

object PostFunction extends MashFunction("http.post") {

  object Params {
    val Url = Parameter(
      name = "url",
      summary = "URL to fetch")
    val Body = Parameter(
      name = "body",
      summary = "Body of post")
    val BasicAuth = Parameter(
      name = "basicAuth",
      summary = "Basic authentication",
      defaultValueGeneratorOpt = Some(() ⇒ MashNull))
    val Json = Parameter(
      name = "json",
      summary = "Whether to post the body as JSON",
      shortFlagOpt = Some('j'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashBoolean.False),
      isBooleanFlag = true)
  }
  import Params._

  val params = ParameterModel(Seq(Url, Body, BasicAuth, Json))

  def apply(arguments: Arguments): MashObject = {
    val boundParams = params.validate(arguments)
    val url = new URI(boundParams.validateString(Url).s)
    val bodyValue = boundParams(Body)
    val json = boundParams(Json).isTruthy

    val request = new HttpPost(url)
    BasicCredentials.getBasicCredentials(boundParams, BasicAuth).foreach(_.addCredentials(request))
    val client = HttpClientBuilder.create().build()
    setBody(request, bodyValue, json)

    val response = client.execute(request)

    GetFunction.asMashObject(response)
  }

  private def setBody(request: HttpEntityEnclosingRequest, bodyValue: MashValue, json: Boolean) {
    val bodyString = if (json) AsJsonFunction.asJsonString(bodyValue) else ToStringifier.stringify(bodyValue)
    val entity = new StringEntity(bodyString, ContentType.APPLICATION_JSON)
    request.setEntity(entity)
  }
  
  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(ResponseClass)

  override def summary = "Make an HTTP POST request"

}