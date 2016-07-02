package com.github.mdr.mash.evaluator

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap
import scala.collection.mutable
import com.github.mdr.mash.Config
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
    val aliasPairs = MashRoot.Aliases.toSeq
    def rootField(s: String) = ns.getField(s).get
    val rootNsPairs = ns.fields.toSeq
    val otherPairs: Seq[(String, MashValue)] =
      Seq(
        Env -> systemEnvironment,
        Config -> com.github.mdr.mash.Config.defaultConfig,
        ReplState.It -> MashNull,
        Ns -> ns)
    val allPairs = nameFunctionPairs ++ aliasPairs ++ rootNsPairs ++ otherPairs
    val global =  MashObject(allPairs, None)
     global.set(Global, global)
    global
  }

  private def systemEnvironment = {
    val fields: Map[String, MashValue] =
      for ((k, v) ← System.getenv.asScala.toMap)
        yield k -> MashString(v)
    MashObject(ListMap(fields.toSeq: _*), classOpt = None)
  }
}

