package com.github.mdr.mash.evaluator

import com.github.mdr.mash.utils.PointedRegion
import com.github.mdr.mash.parser.ConcreteSyntax
import com.github.mdr.mash.parser.Provenance

case class SourceLocation(provenance: Provenance, pointedRegion: PointedRegion)

case class EvaluatorException(
  message: String,
  stack: List[SourceLocation] = Nil,
  cause: Throwable = null)
    extends RuntimeException(message, cause) {
  
  def this(message: String, locationOpt: Option[SourceLocation]) = 
    this(message, locationOpt.toList, null)
  
  def causeOpt: Option[Throwable] = Option(cause)

}
