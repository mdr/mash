package com.github.mdr.mash.functions

import com.github.mdr.mash.inference.{ Type, ValueInfo }

case class BoundTypeParams(boundArguments: Map[Parameter, ValueInfo],
                           boundNames: Map[String, Type],
                           posToParam: Map[Int, Parameter]) {

  def getType(param: Parameter): Option[Type] = boundArguments.get(param).flatMap(_.typeOpt)

  def getArgument(param: Parameter): Option[ValueInfo] = boundArguments.get(param)

  def contains(param: Parameter) = getArgument(param).isDefined

  def paramAt(pos: Int): Option[Parameter] = posToParam get pos

}
