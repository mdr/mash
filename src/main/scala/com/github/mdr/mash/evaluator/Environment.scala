package com.github.mdr.mash.evaluator

import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.runtime.MashObject

case class Environment(globalVariables: MashObject) {

  def get(name: String): Option[MashValue] = globalVariables.get(name)

  def valuesMap: Map[String, MashValue] =
    for ((k, v) ← globalVariables.immutableFields)
      yield k -> v

  def bindings: Map[String, MashValue] = globalVariables.immutableFields

}
