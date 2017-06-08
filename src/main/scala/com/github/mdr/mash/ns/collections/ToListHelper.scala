package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.evaluator.MemberEvaluator
import com.github.mdr.mash.functions.NullaryCallable
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }

object ToListHelper {

  def tryToList(obj: MashObject): Option[Seq[MashValue]] =
    MemberEvaluator.maybeLookup(obj, "toList")
      .collect { case NullaryCallable(nc) ⇒ nc.callNullary() }
      .collect { case xs: MashList ⇒ xs.immutableElements }

}
