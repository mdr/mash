package com.github.mdr.mash.evaluator

import scala.collection.mutable

import com.github.mdr.mash.runtime.MashValue

case class Environment(globalVariables: mutable.Map[String, MashValue]) {

  def get(name: String): Option[MashValue] = globalVariables.get(name)

  def valuesMap: Map[String, MashValue] =
    (for ((k, v) ← globalVariables.toMap) yield k -> v)

  def bindings: Map[String, MashValue] = globalVariables.toMap

}
