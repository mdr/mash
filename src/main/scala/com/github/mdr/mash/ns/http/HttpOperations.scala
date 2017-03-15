package com.github.mdr.mash.ns.http

import java.nio.charset.StandardCharsets

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.ns.json.PrettyPrintFunction
import com.github.mdr.mash.runtime._
import org.apache.commons.io.IOUtils
import org.apache.http.client.CookieStore
import org.apache.http.client.config.{ CookieSpecs, RequestConfig }
import org.apache.http.client.methods.HttpUriRequest
import org.apache.http.cookie.Cookie
import org.apache.http.entity.{ ContentType, StringEntity }
import org.apache.http.impl.client.{ BasicCookieStore, CloseableHttpClient, HttpClients }
import org.apache.http.{ HttpEntityEnclosingRequest, HttpResponse }

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

object HttpOperations {

  def addAcceptApplicationJsonIfRequired(json: Boolean, headers: Seq[Header]): Seq[Header] =
    if (json && !headers.exists(_.name.toLowerCase == "accept"))
      Header("Accept", "application/json") +: headers
    else
      headers

  def runRequest(request: HttpUriRequest,
                 headers: Seq[Header],
                 basicCredentialsOpt: Option[BasicCredentials],
                 bodyValueOpt: Option[MashValue] = None,
                 json: Boolean = false): MashObject = {
    val actualHeaders = addAcceptApplicationJsonIfRequired(json, headers)
    for (header <- actualHeaders)
      request.setHeader(header.name, header.value)
    basicCredentialsOpt.foreach(_.addCredentials(request))

    for (bodyValue ← bodyValueOpt)
      setBody(request.asInstanceOf[HttpEntityEnclosingRequest], bodyValue, json)

    val (cookieStore, client) = makeClient
    val response = client.execute(request)

    asMashObject(response, cookieStore)
  }

  private def makeClient: (BasicCookieStore, CloseableHttpClient) = {
    val cookieStore = new BasicCookieStore
    val client = HttpClients.custom()
      .setDefaultRequestConfig(RequestConfig.custom.setCookieSpec(CookieSpecs.DEFAULT).build())
      .setDefaultCookieStore(cookieStore)
      .setSSLContext(InsecureSsl.makeInsecureSslContext())
      .setSSLHostnameVerifier(InsecureSsl.TrustAllHostnameVerifier)
      .build
    (cookieStore, client)
  }

  private def setBody(request: HttpEntityEnclosingRequest, bodyValue: MashValue, json: Boolean) {
    val bodyString = if (json) PrettyPrintFunction.asJsonString(bodyValue) else ToStringifier.stringify(bodyValue)
    val entity = new StringEntity(bodyString, ContentType.APPLICATION_JSON)
    request.setEntity(entity)
  }

  private def asMashObject(response: HttpResponse, cookieStore: CookieStore): MashObject = {
    val code = response.getStatusLine.getStatusCode
    val content = response.getEntity.getContent
    val responseBody = IOUtils.toString(content, StandardCharsets.UTF_8)
    val headers = asMashObject(response.getAllHeaders)
    val cookies = cookieStore.getCookies.asScala.map(asMashObject(_))
    import ResponseClass.Fields._
    MashObject.of(ListMap(
      Status -> MashNumber(code),
      Body -> MashString(responseBody),
      Headers -> headers,
      Cookies -> MashList(cookies)), ResponseClass)
  }

  private def asMashObject(cookie: Cookie): MashObject = {
    import CookieClass.Fields._
    MashObject.of(ListMap(
      Name -> MashString(cookie.getName),
      Value -> MashString(cookie.getValue)), CookieClass)
  }

  private def asMashObject(headers: Seq[org.apache.http.Header]): MashObject = {
    val pairs =
      for (header ← headers)
        yield header.getName -> MashString(header.getValue)
    MashObject.of(pairs)
  }

}
