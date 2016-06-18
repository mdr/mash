package com.github.mdr.mash.evaluator

import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.time._
import com.github.mdr.mash.ns.os.{ GroupClass ⇒ _, _ }
import com.github.mdr.mash.ns.collections._
import scala.collection.mutable
import scala.collection.JavaConverters._
import com.github.mdr.mash.ns.StandardFunctions
import scala.collection.immutable.ListMap
import scala.collection.mutable.LinkedHashMap
import com.github.mdr.mash.ns.git._
import com.github.mdr.mash.Config
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.ns.core.help.FieldHelpClass
import com.github.mdr.mash.ns.core.help.ClassHelpClass
import com.github.mdr.mash.ns.core.help.ParameterHelpClass
import com.github.mdr.mash.ns.core.help.FunctionHelpClass
import com.github.mdr.mash.ns.time.ChronoUnitClass
import com.github.mdr.mash.ns.time.ChronoUnitClass
import com.github.mdr.mash.ns.view.RawFunction
import com.github.mdr.mash.ns.view.BrowserFunction
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashValue

case class Environment(bindings_ : Map[String, MashValue], globalVariables: mutable.Map[String, MashValue]) {

  def get(name: String): Option[MashValue] = bindings.get(name)

  def addBinding(name: String, value: MashValue) = Environment(bindings + (name -> value), globalVariables)

  def valuesMap: Map[String, MashValue] =
    (for ((k, v) ← globalVariables.toMap) yield k -> v) ++
      (for ((k, v) ← bindings_) yield k -> v)

  def bindings: Map[String, MashValue] = bindings_ ++ globalVariables
}

object Environment {

  def create = Environment(Map(), createGlobalVariables())

  def createGlobalVariables(): mutable.Map[String, MashValue] = {
    val ns = NamespaceCreator.createNamespace
    val nameFunctionPairs = StandardFunctions.StandardFunctions.map(f ⇒ f.name -> f)
    val aliasPairs = StandardFunctions.Aliases.toSeq
    val git = ns.getField("git").get
    val http = ns.getField("http").get
    val json = ns.getField("json").get
    val view = ns.getField("view").get
    val otherPairs = Seq(
      "env" -> systemEnvironment,
      "config" -> Config.defaultConfig,
      "git" -> git,
      "http" -> http,
      "it" -> MashNull,
      "json" -> json,
      "view" -> view,
      "ns" -> ns)
    mutable.Map(nameFunctionPairs ++ aliasPairs ++ otherPairs: _*)
  }

  private def systemEnvironment = {
    val fields: Map[String, MashValue] =
      for ((k, v) ← System.getenv.asScala.toMap)
        yield k -> MashString(v)
    MashObject(ListMap(fields.toSeq: _*), classOpt = None)
  }
}

