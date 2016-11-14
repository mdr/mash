package com.github.mdr.mash.ns.http

import java.nio.charset.Charset

import com.github.mdr.mash.functions.{ BoundParams, Parameter }
import com.github.mdr.mash.runtime.{ MashNull, MashObject, MashString }
import org.apache.commons.codec.binary.Base64
import org.apache.http.{ HttpHeaders, HttpRequest }

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