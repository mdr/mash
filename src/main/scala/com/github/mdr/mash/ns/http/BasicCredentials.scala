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
import com.github.mdr.mash.functions.BoundParams
import org.apache.http.HttpRequest

object BasicCredentials {

  def getBasicCredentials(boundParams: BoundParams, param: Parameter): Option[BasicCredentials] =
    MashNull.option(boundParams(param)).map {
      case MashString(s, _) ⇒
        val Seq(username, password) = s.split(":").take(2).toSeq
        BasicCredentials(username, password)
      case obj: MashObject ⇒
        BasicCredentials(obj("username").asInstanceOf[MashString].s, obj("password").asInstanceOf[MashString].s)
      case value ⇒
        boundParams.throwInvalidArgument(param, "Invalid authorisation of type " + value.typeName)
    }

}

case class BasicCredentials(username: String, password: String) {

  def addCredentials(request: HttpRequest) {
    val auth = username + ":" + password
    val encodedAuth = Base64.encodeBase64(auth.getBytes(Charset.forName("ISO-8859-1")))
    val authHeader = "Basic " + new String(encodedAuth)
    request.setHeader(HttpHeaders.AUTHORIZATION, authHeader)
  }

}