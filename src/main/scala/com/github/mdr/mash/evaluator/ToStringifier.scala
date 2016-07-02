package com.github.mdr.mash.evaluator

import com.github.mdr.mash.functions.MashFunction
import java.time.LocalDate
import java.time.Instant
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.runtime.MashValue
import java.util.function.Supplier
import java.util.IdentityHashMap

object ToStringifier {

  val visited = new ThreadLocal[IdentityHashMap[MashValue, Unit]] {
    override def initialValue = new IdentityHashMap
  }

  def visit[T](x: MashValue, alreadyVisited: T)(f: ⇒ T): T = {
    val map = visited.get
    if (map containsKey x)
      alreadyVisited
    else {
      map.put(x, ())
      try
        f
      finally
        map.remove(x)
    }
  }

  def stringify(x: MashValue): String = {
    val toStringMethod = MemberEvaluator.lookup(x, AnyClass.ToStringMethod.name)
    "" + Evaluator.immediatelyResolveNullaryFunctions(toStringMethod)
  }

}