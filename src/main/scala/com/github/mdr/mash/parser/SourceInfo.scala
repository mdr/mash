package com.github.mdr.mash.parser

import com.github.mdr.mash.evaluator.SourceLocation

case class SourceInfo(provenance: Provenance, node: ConcreteSyntax.AstNode) {

  def location = SourceLocation(provenance, node.pointedRegion)

}

case class Provenance(name: String, source: String) {

  override def toString = s"Provenance($name)"
  
}