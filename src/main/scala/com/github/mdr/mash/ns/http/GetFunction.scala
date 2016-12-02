package com.github.mdr.mash.ns.http

import java.net.URI

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.runtime._
import org.apache.commons.io.IOUtils
import org.apache.http.HttpResponse
import org.apache.http.client.CookieStore
import org.apache.http.client.config.{ CookieSpecs, RequestConfig }
import org.apache.http.client.methods.HttpGet
import org.apache.http.cookie.Cookie
import org.apache.http.impl.client.{ BasicCookieStore, HttpClients }

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

object GetFunction extends MashFunction("http.get") {

  object Params {
    val Url = PostFunction.Params.Url
    val BasicAuth = PostFunction.Params.BasicAuth
    val Headers = PostFunction.Params.Headers
  }

  import Params._

  val params = ParameterModel(Seq(Url, BasicAuth, Headers))

  def apply(arguments: Arguments): MashObject = {
    val boundParams = params.validate(arguments)
    val headers = Header.getHeaders(boundParams, Headers)

    val url = new URI(boundParams.validateString(Url).s)
    val request = new HttpGet(url)
    for (header <- headers)
      request.setHeader(header.name, header.value)
    BasicCredentials.getBasicCredentials(boundParams, BasicAuth).foreach(_.addCredentials(request))

    val cookieStore = new BasicCookieStore
    val customBuilder = HttpClients.custom().setDefaultRequestConfig(RequestConfig.custom.setCookieSpec(CookieSpecs.BROWSER_COMPATIBILITY).build())
    val client = customBuilder.setDefaultCookieStore(cookieStore).setSSLContext(InsecureSsl.makeInsecureSslContext()).setHostnameVerifier(InsecureSsl.TrustAllX509HostnameVerifier).build
    val response = client.execute(request)

    asMashObject(response, cookieStore)
  }

  def asMashObject(response: HttpResponse, cookieStore: CookieStore): MashObject = {
    val code = response.getStatusLine.getStatusCode
    val content = response.getEntity.getContent
    val responseBody = IOUtils.toString(content, "UTF-8")
    val headers = response.getAllHeaders.map(header => asMashObject(header))
    val cookies = cookieStore.getCookies.asScala.map(cookie => asMashObject(cookie))
    import ResponseClass.Fields._
    MashObject.of(ListMap(
      Status -> MashNumber(code),
      Body -> MashString(responseBody),
      Headers -> MashList(headers),
      Cookies -> MashList(cookies)), ResponseClass)
  }

  def asMashObject(cookie: Cookie): MashObject = {
    import CookieClass.Fields._
    MashObject.of(ListMap(
      Name -> MashString(cookie.getName),
      Value -> MashString(cookie.getValue)), CookieClass)
  }

  def asMashObject(header: org.apache.http.Header): MashObject = {
    import HeaderClass.Fields._
    MashObject.of(ListMap(
      Name -> MashString(header.getName),
      Value -> MashString(header.getValue)), HeaderClass)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(ResponseClass)

  override def summary = "Make an HTTP GET request"

}