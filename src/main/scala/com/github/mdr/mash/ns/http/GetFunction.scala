package com.github.mdr.mash.ns.http

import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.inference.Type
import org.apache.http.impl.client.HttpClients
import org.apache.http.client.methods.HttpGet
import java.net.URI
import org.apache.commons.io.IOUtils
import scala.collection.immutable.ListMap
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashNull
import org.apache.http.auth.UsernamePasswordCredentials
import org.apache.http.impl.client.BasicCredentialsProvider
import org.apache.http.auth.AuthScope
import org.apache.http.impl.client.HttpClientBuilder

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
    val credentialsProvider = new BasicCredentialsProvider
    val clientBuilder = HttpClientBuilder.create()
    for (BasicCredentials(username, password) ← basicCredentialsOpt) {
      credentialsProvider.setCredentials(AuthScope.ANY, new UsernamePasswordCredentials(username, password))
      clientBuilder.setDefaultCredentialsProvider(credentialsProvider);
    }
    val client = clientBuilder.build()
    val httpGet = new HttpGet(new URI(url.s))
    val response = client.execute(httpGet)
    val code = response.getStatusLine.getStatusCode
    val content = response.getEntity.getContent
    val body = IOUtils.toString(content, "UTF-8")
    import ResponseClass.Fields._
    MashObject.of(ListMap(
      Status -> MashNumber(code),
      Body -> MashString(body)), ResponseClass)

  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(ResponseClass)

  override def summary = "Make an HTTP GET request"

}