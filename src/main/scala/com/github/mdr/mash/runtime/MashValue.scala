package com.github.mdr.mash.runtime

import com.github.mdr.mash.evaluator.BoundMethod
import java.time.Instant
import com.github.mdr.mash.functions.MashFunction
import java.time.LocalDate
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.evaluator.MashClass

object MashValue {

  def checkIsValidRuntimeValue(x: MashValue) = x match {
    case MashNull | MashUnit | MashBoolean.True | MashBoolean.False | MashString(_, _) | MashNumber(_, _) | MashObject(_, _) ⇒
    case _: MashFunction | _: BoundMethod ⇒
    case _: MashClass ⇒
    case MashWrapped(_: Instant | _: LocalDate) ⇒
    case xs: MashList ⇒
    case _ ⇒ throw EvaluatorException("Unexpected runtime type: " + x.getClass)
  }

}

trait MashValue {

}