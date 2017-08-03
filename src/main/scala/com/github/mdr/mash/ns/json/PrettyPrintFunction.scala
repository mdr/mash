package com.github.mdr.mash.ns.json

import java.io.StringWriter

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime._
import com.google.gson._
import com.google.gson.internal.Streams
import com.google.gson.stream.JsonWriter

object PrettyPrintFunction extends MashFunction("json.prettyPrint") {

  object Params {

    val Compact = Parameter(
      nameOpt = Some("compact"),
      shortFlagOpt = Some('c'),
      summaryOpt = Some("Pretty print to a single line (default false)"),
      defaultValueGeneratorOpt = Some(false),
      isFlag = true,
      isBooleanFlag = true)

    val Value = Parameter(
      nameOpt = Some("value"),
      summaryOpt = Some("Value to convert to a JSON string"))

  }

  import Params._

  val params = ParameterModel(Compact, Value)

  def call(boundParams: BoundParams): MashString = {
    val value = boundParams(Value)
    val compact = boundParams(Compact).isTruthy
    MashString(asJsonString(value, compact))
  }

  def asJsonString(value: MashValue, compact: Boolean = false): String = {
    val json = asJson(value)
    val stringWriter = new StringWriter
    val jsonWriter = new JsonWriter(stringWriter)
    jsonWriter.setLenient(true)
    jsonWriter.setIndent(if (compact) "" else "  ")
    Streams.write(json, jsonWriter)
    stringWriter.toString
  }

  private def isValidLong(n: Double) = {
    val l = n.toLong; l.toDouble == n && l != Long.MaxValue
  }

  def asJson(value: MashValue): JsonElement = value match {
    case MashBoolean.True  ⇒ new JsonPrimitive(true)
    case MashBoolean.False ⇒ new JsonPrimitive(false)
    case n: MashNumber     ⇒
      if (isValidLong(n.n))
        new JsonPrimitive(n.n.toLong)
      else
        new JsonPrimitive(n.n)
    case MashString(s, _)  ⇒ new JsonPrimitive(s)
    case MashNull          ⇒ JsonNull.INSTANCE
    case list: MashList    ⇒
      val array = new JsonArray
      for (item ← list.elements)
        array.add(asJson(item))
      array
    case obj: MashObject   ⇒
      val jsonObj = new JsonObject
      for ((field, value) ← obj.immutableFields)
        jsonObj.add(ToStringifier.stringify(field), asJson(value))
      jsonObj
    case _                 ⇒
      new JsonPrimitive(ToStringifier.stringify(value))
  }

  override def typeInferenceStrategy = StringClass

  override def summaryOpt = Some("Convert the given value to a pretty-printed JSON string")
}

