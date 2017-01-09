package com.github.mdr.mash.functions

import com.github.mdr.mash.inference.{ ValueInfo, Type }

case class BoundTypeParams(boundArguments: Map[String, ValueInfo],
                           boundNames: Map[String, Type],
                           posToParam: Map[Int, Parameter]) {

  def getType(param: Parameter): Option[Type] = param.nameOpt.flatMap(boundNames.get)

  def getArgument(param: Parameter): Option[ValueInfo] = param.nameOpt.flatMap(boundArguments.get)

  def contains(param: Parameter) = getArgument(param).isDefined

  def paramAt(pos: Int): Option[Parameter] = posToParam get pos

}
