package com.github.mdr.mash.ns.http

import java.nio.charset.Charset

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, Parameter }
import com.github.mdr.mash.ns.core.NoArgFunction
import com.github.mdr.mash.runtime.{ MashObject, MashString }
import org.apache.commons.codec.binary.Base64
import org.apache.http.{ HttpHeaders, HttpRequest }

object BasicCredentials {

  def getBasicCredentials(boundParams: BoundParams, param: Parameter): Option[BasicCredentials] =
    NoArgFunction.option(boundParams(param)).map {
      case MashString(s, _) ⇒
        val chunks = s.split(":").toSeq
        if (chunks.length != 2)
          boundParams.throwInvalidArgument(param, "Auth string must contain a single ':'")
        val Seq(username, password) = s.split(":").take(2).toSeq
        BasicCredentials(username, password)
      case obj: MashObject  ⇒
        def get(fieldName: String): String =
          ToStringifier.stringify(obj.get(fieldName).getOrElse(
            boundParams.throwInvalidArgument(param, s"Auth object must have '$fieldName' field")))
        BasicCredentials(get("username"), get("password"))
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