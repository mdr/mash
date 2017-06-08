package com.github.mdr.mash.classes

import com.github.mdr.mash.functions.{ MashCallable, MashMethod, NullaryCallable }
import com.github.mdr.mash.runtime.MashValue

case class BoundMethod(target: MashValue, method: MashMethod, klass: MashClass) extends MashValue with MashCallable with NullaryCallable {

  def allowsNullary = method.allowsNullary

  def callNullary() = method.callNullary(target)

  def fullyQualifiedName = klass.fullyQualifiedName + "." + method.name

  def name = method.name

  override def toString = fullyQualifiedName

}

