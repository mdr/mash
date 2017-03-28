package com.github.mdr.mash.ns.http

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.json.FromFileFunction.parseJson
import com.github.mdr.mash.runtime._

object ResponseClass extends MashClass("http.Response") {

  object Fields {
    lazy val Status = Field("status", Some("Status code"), NumberClass)
    lazy val Body = Field("body", Some("Body of the response"), StringClass)
    lazy val Headers = Field("headers", Some("Response headers"), ObjectClass)
    lazy val Cookies = Field("cookies", Some("Response cookies"), Seq(CookieClass))
  }

  import Fields._

  override val fields = Seq(Status, Body, Headers, Cookies)

  override val methods = Seq(
    GetHeadersMethod,
    HeadersObjectMethod,
    JsonMethod,
    SucceededMethod)

  override val staticMethods = Seq(NewStaticMethod(this))

  case class Wrapper(value: MashValue) extends AbstractObjectWrapper(value) {
    def body = getStringField(Body)

    def code = getNumberField(Status)

    def headers: Seq[Header] = getListField(Headers).map(HeaderClass.asHeader)
  }

  object JsonMethod extends MashMethod("json") {

    val params = ParameterModel()

    def apply(target: MashValue, boundParams: BoundParams): MashValue = {
      parseJson(Wrapper(target).body)
    }

    override def typeInferenceStrategy = ObjectClass

    override def summaryOpt = Some("Parse response body as JSON")

  }

  object SucceededMethod extends MashMethod("succeeded") {

    val params = ParameterModel()

    def apply(target: MashValue, boundParams: BoundParams): MashBoolean = {
      val code = Wrapper(target).code
      MashBoolean(200 <= code && code <= 299)
    }

    override def typeInferenceStrategy = BooleanClass

    override def summaryOpt = Some("True if the HTTP request succeeded (status code 2xx)")

  }

  object HeadersObjectMethod extends MashMethod("headersObject") {

    val params = ParameterModel()

    def apply(target: MashValue, boundParams: BoundParams): MashObject = {
      val pairs =
        for (Header(name, value) <- Wrapper(target).headers)
          yield name -> MashString(value)
      MashObject.of(pairs)
    }

    override def typeInferenceStrategy = Seq(HeaderClass)

    override def summaryOpt = Some("The headers as an object")

  }

  object GetHeadersMethod extends MashMethod("getHeaders") {

    object Params {
      val Name = Parameter(
        nameOpt = Some("name"),
        summaryOpt = Some("Header name to search for"))
    }

    import Params._

    val params = ParameterModel(Seq(Name))

    def apply(target: MashValue, boundParams: BoundParams): MashList = {
      val requestedName = boundParams.validateString(Name).s
      val elements =
        for (Header(name, value) <- Wrapper(target).headers if name.toLowerCase == requestedName)
          yield MashString(value)
      MashList(elements)
    }

    override def typeInferenceStrategy = Seq(StringClass)

    override def summaryOpt = Some("Get all headers with the given name, ignoring case")

  }

  override def summaryOpt = Some("An HTTP response")

}