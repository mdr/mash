package com.github.mdr.mash.ns.http

import java.net.URI
import scala.collection.immutable.ListMap
import org.apache.commons.io.IOUtils
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.HttpClientBuilder
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.ns.http.ResponseClass.Fields
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashValue
import org.apache.http.HttpResponse

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

  def apply(arguments: Arguments): MashObject = {
    val boundParams = params.validate(arguments)
    
    val url = new URI(boundParams.validateString(Url).s)
    val request = new HttpGet(url)
    BasicCredentials.getBasicCredentials(boundParams, BasicAuth).foreach(_.addCredentials(request))
    val client = HttpClientBuilder.create().build()

    val response = client.execute(request)

    asMashObject(response)
  }

  def asMashObject(response: HttpResponse): MashObject = {
    val code = response.getStatusLine.getStatusCode
    val content = response.getEntity.getContent
    val responseBody = IOUtils.toString(content, "UTF-8")
    import ResponseClass.Fields._
    MashObject.of(ListMap(
      Status -> MashNumber(code),
      Body -> MashString(responseBody)), ResponseClass)    
  }
  
  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(ResponseClass)

  override def summary = "Make an HTTP GET request"

}