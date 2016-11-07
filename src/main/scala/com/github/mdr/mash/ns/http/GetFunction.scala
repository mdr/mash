package com.github.mdr.mash.ns.http

import java.net.URI
import scala.collection.immutable.ListMap
import org.apache.commons.io.IOUtils
import org.apache.commons.codec.binary.Base64
import org.apache.http.HttpHeaders
import org.apache.http.auth.AuthScope
import org.apache.http.auth.UsernamePasswordCredentials
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.BasicCredentialsProvider
import org.apache.http.impl.client.HttpClientBuilder
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type.classToType
import com.github.mdr.mash.ns.http.ResponseClass.Fields
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashValue

import java.nio.charset.Charset

case class BasicCredentials(username: String, password: String)

object GetFunction extends MashFunction("http.get") {

  object Params {
    val Url = Parameter(
      name = "url",
      summary = "URL to fetch")
    val BasicAuth = Parameter(
      name = "basicAuth",
      summary = "Basic authentication",
      defaultValueGeneratorOpt = Some(() ⇒ MashNull))
  }
  import Params._

  val params = ParameterModel(Seq(Url, BasicAuth))

  private def getBasicCredentials(arg: MashValue): Option[BasicCredentials] =
    MashNull.option(arg).map {
      case MashString(s, _) ⇒
        val Seq(username, password) = s.split(":").take(2).toSeq
        BasicCredentials(username, password)
      case obj: MashObject ⇒
        BasicCredentials(obj("username").asInstanceOf[MashString].s, obj("password").asInstanceOf[MashString].s)
    }

  def apply(arguments: Arguments): MashObject = {
    val boundParams = params.validate(arguments)
    val url = boundParams.validateString(Url)

    val basicCredentialsOpt = getBasicCredentials(boundParams(BasicAuth))
    val clientBuilder = HttpClientBuilder.create()
    val request = new HttpGet(new URI(url.s))
    for (BasicCredentials(username, password) ← basicCredentialsOpt)
      addCredentials(request, clientBuilder, username, password)
    val client = clientBuilder.build()
    
    val response = client.execute(request)
    
    val code = response.getStatusLine.getStatusCode
    val content = response.getEntity.getContent
    val body = IOUtils.toString(content, "UTF-8")
    import ResponseClass.Fields._
    MashObject.of(ListMap(
      Status -> MashNumber(code),
      Body -> MashString(body)), ResponseClass)
  }
  
  private def addCredentials(request: HttpGet, clientBuilder: HttpClientBuilder, username: String, password: String) {
    val credentialsProvider = new BasicCredentialsProvider
      credentialsProvider.setCredentials(AuthScope.ANY, new UsernamePasswordCredentials(username, password))
      clientBuilder.setDefaultCredentialsProvider(credentialsProvider)
      val auth = username + ":" + password
      val encodedAuth = Base64.encodeBase64(auth.getBytes(Charset.forName("ISO-8859-1")))
       val authHeader = "Basic " + new String(encodedAuth)
      request.setHeader(HttpHeaders.AUTHORIZATION, authHeader)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(ResponseClass)

  override def summary = "Make an HTTP GET request"

}