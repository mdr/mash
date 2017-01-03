package com.github.mdr.mash.functions

import com.github.mdr.mash.inference.{ AnnotatedExpr, Type }

case class BoundTypeParams(boundArguments: Map[String, AnnotatedExpr],
                           boundNames: Map[String, Type],
                           posToParam: Map[Int, Parameter]) {

  def getType(param: Parameter): Option[Type] = boundNames get param.name

  def getArgument(param: Parameter): Option[AnnotatedExpr] = boundArguments get param.name

  def contains(param: Parameter) = getArgument(param).isDefined

  def paramAt(pos: Int): Option[Parameter] = posToParam get pos

}
