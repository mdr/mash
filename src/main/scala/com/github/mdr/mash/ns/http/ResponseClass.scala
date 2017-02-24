package com.github.mdr.mash.ns.http

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.inference.Type.classToType
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.json.FromFileFunction
import com.github.mdr.mash.ns.json.FromFileFunction.parseJson
import com.github.mdr.mash.runtime._

object ResponseClass extends MashClass("http.Response") {

  object Fields {
    lazy val Status = Field("status", Some("Status code"), NumberClass)
    lazy val Body = Field("body", Some("Body of the response"), StringClass)
    lazy val Headers = Field("headers", Some("Response headers"), Seq(HeaderClass))
    lazy val Cookies = Field("cookies", Some("Response cookies"), Seq(CookieClass))
  }

  import Fields._

  override val fields = Seq(Status, Body, Headers, Cookies)

  override val methods = Seq(
    JsonMethod,
    SucceededMethod)

  override val staticMethods = Seq(NewStaticMethod(this))

  case class Wrapper(value: MashValue) extends AbstractObjectWrapper(value) {
    def body = getStringField(Body)
    def code = getNumberField(Status)
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
  
  override def summaryOpt = Some("An HTTP response")

}