package com.github.mdr.mash.evaluator

import scala.collection.JavaConverters._
import com.github.mdr.mash.ns.MashRoot
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.repl.ReplState

object StandardEnvironment {

  val Env = "env"
  val Config = "config"
  val Global = "global"
  val Ns = "ns"

  def create = Environment(createGlobalVariables())

  def createGlobalVariables(): MashObject = {
    val ns = NamespaceCreator.createNamespace
    val nameFunctionPairs = MashRoot.StandardFunctions.map(f ⇒ f.name -> f)
    val nameClassPairs = MashRoot.StandardClasses.map(c => c.name -> c)
    val aliasPairs = MashRoot.Aliases.toSeq
    def rootField(s: String) = ns.get(s).get
    val rootNsPairs = ns.fields.toSeq
    val otherPairs: Seq[(String, MashValue)] =
      Seq(
        Env -> systemEnvironment,
        Config -> com.github.mdr.mash.Config.defaultConfig,
        ReplState.It -> MashNull,
        Ns -> ns)
    val allPairs = nameFunctionPairs ++ nameClassPairs ++ aliasPairs ++ rootNsPairs ++ otherPairs
    val global = MashObject.of(allPairs)
    global.set(Global, global)
    global
  }

  private def systemEnvironment = {
    val fields: Map[String, MashValue] =
      for ((k, v) ← System.getenv.asScala.toMap)
        yield k -> MashString(v)
    MashObject.of(fields)
  }
}

