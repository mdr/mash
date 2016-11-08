package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.functions.MashCallable

case class BoundMethod(target: MashValue, method: MashMethod, klass: MashClass) extends MashValue with MashCallable {

  def fullyQualifiedName = klass.fullyQualifiedName + "." + method.name

  def name = method.name

  override def toString = fullyQualifiedName

}

