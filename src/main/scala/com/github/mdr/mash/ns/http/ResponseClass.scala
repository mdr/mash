package com.github.mdr.mash.ns.http

import com.github.mdr.mash.evaluator.{ Arguments, Field, MashClass }
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantMethodTypeInferenceStrategy, Type }
import com.github.mdr.mash.inference.Type.classToType
import com.github.mdr.mash.ns.core.{ BooleanClass, NumberClass, StringClass }
import com.github.mdr.mash.ns.json.FromFileFunction
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

  object JsonMethod extends MashMethod("json") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      val body = target.asInstanceOf[MashObject].apply(Body).asInstanceOf[MashString].s
      FromFileFunction.parseJson(body)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Any)

    override def summaryOpt = Some("Parse response body as JSON")

  }
  
  object SucceededMethod extends MashMethod("succeeded") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      val code = target.asInstanceOf[MashObject].apply(Status).asInstanceOf[MashNumber].n
      MashBoolean(200 <= code && code <= 299)
    }

    override def typeInferenceStrategy = BooleanClass

    override def summaryOpt = Some("True if the HTTP request succeeded (status code 2xx)")

  }
  
  override def summaryOpt = Some("An HTTP response")

}