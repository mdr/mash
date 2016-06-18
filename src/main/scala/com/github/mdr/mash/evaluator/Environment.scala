package com.github.mdr.mash.evaluator

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap
import scala.collection.mutable

import com.github.mdr.mash.Config
import com.github.mdr.mash.ns.MashRoot
import com.github.mdr.mash.runtime._

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
    val nameFunctionPairs = MashRoot.StandardFunctions.map(f ⇒ f.name -> f)
    val aliasPairs = MashRoot.Aliases.toSeq
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

