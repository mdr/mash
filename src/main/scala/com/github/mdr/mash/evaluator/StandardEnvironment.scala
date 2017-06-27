package com.github.mdr.mash.evaluator

import com.github.mdr.mash.ns.MashRoot
import com.github.mdr.mash.repl.ReplState
import com.github.mdr.mash.runtime._

import scala.collection.JavaConverters._

object StandardEnvironment {

  val Env = "env"
  val Config = "config"
  val Global = "global"
  val Ns = "ns"

  def create = Environment(createGlobalVariables())

  def createGlobalVariables(): MashObject = {
    val ns = NamespaceCreator.createNamespace
    val nameFunctionPairs = MashRoot.StandardFunctions.map(f ⇒ MashString(f.name) -> f)
    val nameClassPairs = MashRoot.StandardClasses.map(c ⇒ MashString(c.name) -> c)
    val aliasPairs = MashRoot.Aliases.toSeq.map { case (k, v) ⇒ MashString(k) -> v }
    val rootNsPairs = ns.fields.toSeq
    val otherPairs =
      Seq(
        Env -> systemEnvironment,
        Config -> com.github.mdr.mash.Config.defaultConfig,
        ReplState.It -> MashNull,
        Ns -> ns).map { case (k, v) ⇒ MashString(k) -> v } // TODO_OBJ
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

