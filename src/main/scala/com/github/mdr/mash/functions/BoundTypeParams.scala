package com.github.mdr.mash.functions

import com.github.mdr.mash.inference.{ Type, ValueInfo }
import com.github.mdr.mash.runtime.{ MashString, MashValue }

case class BoundTypeParams(boundArguments: Map[Parameter, ValueInfo],
                           boundNames: Map[String, Type],
                           posToParam: Map[Int, Parameter]) {

  def getType(param: Parameter): Option[Type] = boundArguments.get(param).flatMap(_.typeOpt)

  def getArgument(param: Parameter): Option[ValueInfo] = boundArguments.get(param)

  def getValue(param: Parameter): Option[MashValue] = getArgument(param).flatMap(_.valueOpt)

  def getStringValue(param: Parameter): Option[String] =
    getValue(param).flatMap {
      case MashString(s, _) ⇒ Some(s)
      case _                ⇒ None
    }

  def getBooleanValue(param: Parameter): Option[Boolean] = getValue(param).map(_.isTruthy)

  def contains(param: Parameter): Boolean = getArgument(param).isDefined

  def paramAt(pos: Int): Option[Parameter] = posToParam get pos

}
