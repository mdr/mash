package com.github.mdr.mash.runtime

import scala.PartialFunction.condOpt
import java.time.LocalDate
import java.time.Instant
import com.github.mdr.mash.evaluator.EvaluatorException

trait MashValue

object MashValueOrdering extends Ordering[MashValue] {

  override def compare(v1: MashValue, v2: MashValue) = compareOpt(v1, v2) getOrElse (
    throw new EvaluatorException("Incomparable values"))

  def compareOpt(v1: MashValue, v2: MashValue): Option[Int] =
    condOpt((v1, v2)) {
      case (n1: MashNumber, n2: MashNumber)                         ⇒ n1 compareTo n2
      case (s1: MashString, s2: MashString)                         ⇒ s1 compareTo s2
      case (MashWrapped(d1: LocalDate), MashWrapped(d2: LocalDate)) ⇒ d1 compareTo d2
      case (MashWrapped(t1: Instant), MashWrapped(t2: Instant))     ⇒ t1 compareTo t2
    }

}