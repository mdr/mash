package com.github.mdr.mash.ns.http

import java.net.URI

import com.github.mdr.mash.evaluator.{ Arguments, ToStringifier }
import com.github.mdr.mash.functions.{ MashFunction, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.json.PrettyPrintFunction
import com.github.mdr.mash.runtime._
import org.apache.http.HttpEntityEnclosingRequest
import org.apache.http.client.methods.{ HttpDelete, HttpPut }
import org.apache.http.entity.{ ContentType, StringEntity }
import org.apache.http.impl.client.{ BasicCookieStore, HttpClientBuilder }

object DeleteFunction extends MashFunction("http.delete") {

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

    val request = new HttpDelete(url)
    for (header <- headers)
      request.setHeader(header.name, header.value)
    BasicCredentials.getBasicCredentials(boundParams, BasicAuth).foreach(_.addCredentials(request))
    val cookieStore = new BasicCookieStore
    val client = HttpClientBuilder.create
      .setDefaultCookieStore(cookieStore)
      .setSSLContext(InsecureSsl.makeInsecureSslContext())
      .setSSLHostnameVerifier(InsecureSsl.TrustAllHostnameVerifier)
      .build

    val response = client.execute(request)

    GetFunction.asMashObject(response, cookieStore)
  }

  override def typeInferenceStrategy = ResponseClass

  override def summaryOpt = Some("Make an HTTP DELETE request")

}