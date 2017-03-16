package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.EvaluationContext
import com.github.mdr.mash.runtime._

import scala.language.implicitConversions

trait ValueGenerator {

  def generate(context: EvaluationContext): MashValue

}

object ValueGenerator {

  implicit def fromThunk(gen: () ⇒ MashValue): ValueGenerator = context ⇒ gen()
  implicit def fromBoolean(b: MashBoolean): ValueGenerator = context ⇒ b
  implicit def fromNumber(n: MashNumber): ValueGenerator = context ⇒ n
  implicit def fromString(s: MashString): ValueGenerator = context ⇒ s
  implicit def fromNull(n: MashNull.type): ValueGenerator = context ⇒ n
  implicit def fromUnit(u: MashUnit.type): ValueGenerator = context ⇒ u
  implicit def fromFun(gen: EvaluationContext ⇒ MashValue): ValueGenerator = context ⇒ gen(context)

}
