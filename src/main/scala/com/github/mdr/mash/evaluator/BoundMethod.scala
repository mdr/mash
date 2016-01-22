package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.parser.AbstractSyntax._

case class BoundMethod(target: Any, method: MashMethod, klass: MashClass) {

  def fullyQualifiedName = klass.fullyQualifiedName + "." + method.name

  def name = method.name

  override def toString = fullyQualifiedName

}

