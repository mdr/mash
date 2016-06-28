package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions.MashFunction
import java.time.LocalDate
import java.time.Instant
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.runtime.MashValue

object ToStringifier {

  def stringify(x: MashValue): String = {
    val toStringMethod = MemberEvaluator.lookup(x, AnyClass.ToStringMethod.name)
    "" + Evaluator.immediatelyResolveNullaryFunctions(toStringMethod)
  }

}