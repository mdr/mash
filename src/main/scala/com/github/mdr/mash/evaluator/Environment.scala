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

case class Environment(bindings: Map[String, Any], globalVariables: mutable.Map[String, Any]) {

  def get(name: String): Option[Any] = bindings.get(name)

  def addBinding(name: String, value: Any) = Environment(bindings + (name -> value), globalVariables)

  def valuesMap: Map[String, Any] = {
    (for ((k, v) ← globalVariables.toMap) yield k -> v) ++
      (for ((k, v) ← bindings) yield k -> v)
  }

}

object Environment {

  def create = Environment(Map(), createGlobalVariables())

  def createGlobalVariables(): mutable.Map[String, Any] = {
    val nameFunctionPairs = StandardFunctions.Functions.map(f ⇒ f.name -> f)
    val aliasPairs = StandardFunctions.Aliases.toSeq
    
    val gitBindings = for (gitFunction ← StandardFunctions.GitFunctions) yield gitFunction.name -> gitFunction
    val otherPairs = Seq(
      "env" -> systemEnvironment,
      "config" -> Config.defaultConfig,
      "git" -> MashObject(LinkedHashMap(gitBindings: _*)),
      "view" -> MashObject(LinkedHashMap("raw" -> RawFunction)),
      "ns" -> NamespaceCreator.createNamespace)
    mutable.Map(nameFunctionPairs ++ aliasPairs ++ otherPairs: _*)
  }

  private def systemEnvironment = {
    val fields: Map[String, Any] =
      for ((k, v) ← System.getenv.asScala.toMap)
        yield k -> MashString(v)
    MashObject(ListMap(fields.toSeq: _*), classOpt = None)
  }
}

