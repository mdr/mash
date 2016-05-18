package com.github.mdr.mash.functions

import com.github.mdr.mash.inference.AnnotatedExpr

case class BoundTypeParams(params: Map[String, AnnotatedExpr], posToParam: Map[Int, Parameter]) {

  def apply(param: String): AnnotatedExpr = params(param)

  def apply(param: Parameter): AnnotatedExpr = params(param.name)

  def get(param: Parameter): Option[AnnotatedExpr] = params.get(param.name)

  def get(param: String): Option[AnnotatedExpr] = params.get(param)

  def contains(param: String) = get(param).isDefined

  def contains(param: Parameter) = get(param).isDefined
  
  def paramAt(pos: Int): Option[Parameter] = posToParam.get(pos)
  
}
