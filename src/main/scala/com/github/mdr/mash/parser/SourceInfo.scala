package com.github.mdr.mash.parser

import com.github.mdr.mash.evaluator.SourceLocation

case class SourceInfo(provenance: Provenance, expr: ConcreteSyntax.AstNode) {

  def location = SourceLocation(provenance, expr.pointedRegion)

}

case class Provenance(name: String, source: String)