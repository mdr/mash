package com.github.mdr.mash.evaluator

import com.github.mdr.mash.runtime.{ MashObject, MashValue }

case class Environment(globalVariables: MashObject) {

  def get(name: String): Option[MashValue] = globalVariables.get(name)

  def bindings: Map[String, MashValue] = globalVariables.stringFields

}
