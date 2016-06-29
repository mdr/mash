package com.github.mdr.mash.evaluator

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap
import scala.collection.mutable

import com.github.mdr.mash.Config
import com.github.mdr.mash.ns.MashRoot
import com.github.mdr.mash.runtime._

object StandardEnvironment {

  def create = Environment(createGlobalVariables())

  def createGlobalVariables(): mutable.Map[String, MashValue] = {
    val ns = NamespaceCreator.createNamespace
    val nameFunctionPairs = MashRoot.StandardFunctions.map(f ⇒ f.name -> f)
    val aliasPairs = MashRoot.Aliases.toSeq
    def rootField(s: String) = ns.getField(s).get
    val rootNsPairs = ns.fields.toSeq
    val otherPairs =
      Seq(
        "env" -> systemEnvironment,
        "config" -> Config.defaultConfig,
        "it" -> MashNull,
        "ns" -> ns)
    mutable.Map(nameFunctionPairs ++ aliasPairs ++ rootNsPairs ++ otherPairs: _*)
  }

  private def systemEnvironment = {
    val fields: Map[String, MashValue] =
      for ((k, v) ← System.getenv.asScala.toMap)
        yield k -> MashString(v)
    MashObject(ListMap(fields.toSeq: _*), classOpt = None)
  }
}

