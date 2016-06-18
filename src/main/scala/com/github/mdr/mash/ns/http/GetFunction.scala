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

object GetFunction extends MashFunction("http.get") {

  object Params {
    val Url = Parameter(
      name = "url",
      summary = "URL to fetch")
  }
  import Params._

  val params = ParameterModel(Seq(Url))

  def apply(arguments: Arguments): MashObject = {
    val boundParams = params.validate(arguments)
    val url = boundParams.validateString(Url)
    val httpclient = HttpClients.createDefault
    val httpGet = new HttpGet(new URI(url.s))
    val response = httpclient.execute(httpGet)
    val code = response.getStatusLine.getStatusCode
    val is = response.getEntity.getContent
    val body = IOUtils.toString(is, "UTF-8")
    import ResponseClass.Fields._
    MashObject(ListMap(
      Status -> MashNumber(code),
      Body -> MashString(body)), ResponseClass)

  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(ResponseClass)

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summary = "Make an HTTP request "

}  