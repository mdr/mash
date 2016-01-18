package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions.MashFunction
import java.time.LocalDate
import java.time.Instant
import com.github.mdr.mash.ns.core.ObjectClass

object ToStringifier {

  def stringify(x: Any): String = {
    val toStringMethod = MemberEvaluator.lookup(x, ObjectClass.ToStringMethod.name)
    "" + Evaluator.immediatelyResolveNullaryFunctions(toStringMethod)
  }

}