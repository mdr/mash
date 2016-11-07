package com.github.mdr.mash.ns.http

import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.inference.Type.classToType
import com.github.mdr.mash.ns.core.NumberClass
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.ns.json.FromFileFunction
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashBoolean
import com.github.mdr.mash.ns.core.BooleanClass

object ResponseClass extends MashClass("http.Response") {

  object Fields {
    lazy val Status = Field("status", "Status code", NumberClass)
    lazy val Body = Field("body", "Body of the response", StringClass)
  }

  import Fields._

  override val fields = Seq(Status, Body)

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

    override def summary = "Parse response body as JSON"

  }
  
  object SucceededMethod extends MashMethod("succeeded") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      params.validate(arguments)
      val code = target.asInstanceOf[MashObject].apply(Status).asInstanceOf[MashNumber].n
      MashBoolean(200 <= code && code <= 299)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "True if the HTTP request succeeded (status code 2xx)"

  }
  
  override def summary = "An HTTP response"

}