package com.github.mdr.mash.evaluator

import com.github.mdr.mash.utils.PointedRegion

case class SourceLocation(pointedRegion: PointedRegion)

case class EvaluatorException(
  message: String,
  locationOpt: Option[SourceLocation] = None,
  cause: Throwable = null)
    extends RuntimeException(message, cause) {

  def causeOpt: Option[Throwable] = Option(cause)

}
