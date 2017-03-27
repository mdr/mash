package com.github.mdr.mash.ns.http

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.ns.json.PrettyPrintFunction
import com.github.mdr.mash.runtime._
import org.apache.commons.io.IOUtils
import org.apache.http.client.CookieStore
import org.apache.http.client.config.{ CookieSpecs, RequestConfig }
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.HttpUriRequest
import org.apache.http.client.utils.URLEncodedUtils
import org.apache.http.cookie.Cookie
import org.apache.http.entity.{ ContentType, FileEntity, StringEntity }
import org.apache.http.impl.client.{ BasicCookieStore, CloseableHttpClient, HttpClients }
import org.apache.http.message.BasicNameValuePair
import org.apache.http.{ HttpEntityEnclosingRequest, HttpResponse }

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

sealed trait BodySource

object BodySource {

  case class Value(value: MashValue) extends BodySource

  case class File(path: Path) extends BodySource

}

object HttpOperations {

  def addAcceptApplicationJsonIfRequired(json: Boolean, headers: Seq[Header]): Seq[Header] =
    if (json && !headers.exists(_.name.toLowerCase == "accept"))
      Header("Accept", "application/json") +: headers
    else
      headers

  /**
    * @param form  if set, body source must be a Value(MashObject)
    */
  def runRequest(request: HttpUriRequest,
                 headers: Seq[Header],
                 basicCredentialsOpt: Option[BasicCredentials],
                 bodySourceOpt: Option[BodySource] = None,
                 json: Boolean = false,
                 form: Boolean = false): MashObject = {
    val actualHeaders = addAcceptApplicationJsonIfRequired(json, headers)
    for (header <- actualHeaders)
      request.setHeader(header.name, header.value)
    basicCredentialsOpt.foreach(_.addCredentials(request))

    for (bodySource ← bodySourceOpt)
      setBody(request.asInstanceOf[HttpEntityEnclosingRequest], bodySource, json, form)

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

  private def setBody(request: HttpEntityEnclosingRequest, bodySource: BodySource, json: Boolean, form: Boolean) {
    if (form) {
      val entity = bodySource match {
        case BodySource.Value(obj: MashObject) ⇒
          import scala.collection.JavaConverters._
          val nameValues = obj.immutableFields.map { case (key, value) ⇒
            new BasicNameValuePair(key, ToStringifier.stringify(value))
          }.asJava
          new UrlEncodedFormEntity(nameValues)
        case _                                 ⇒
          throw new AssertionError(s"Unexpected body source: $bodySource")
      }
      request.setEntity(entity)
    } else {
      val contentType = if (json) ContentType.APPLICATION_JSON else ContentType.APPLICATION_OCTET_STREAM

      val entity = bodySource match {
        case BodySource.Value(value) ⇒
          val bodyString = if (json) PrettyPrintFunction.asJsonString(value) else ToStringifier.stringify(value)
          new StringEntity(bodyString, contentType)
        case BodySource.File(path)   ⇒
          new FileEntity(path.toFile, contentType)
      }
      request.setEntity(entity)
    }
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
