package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.evaluator.{ EvaluatorException, ToStringifier }
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.runtime.{ MashList, MashObject, MashValue }

object FromPairsStaticMethod extends MashFunction("fromPairs") {

  object Params {
    val Pairs = Parameter(
      nameOpt = Some("pairs"),
      summaryOpt = Some("Pairs to merge into an object"))
  }

  import Params._

  val params = ParameterModel(Seq(Pairs))

  override def apply(boundParams: BoundParams): MashObject = {
    val xs = boundParams.validateSequence(Pairs, allowStrings = false)
    val pairs =
      for (x ← xs)
        yield makePair(x)
    MashObject.of(pairs)
  }

  private def makePair(value: MashValue): (String, MashValue) = value match {
    case xs: MashList    ⇒
      xs.immutableElements match {
        case Seq()                  ⇒ throw new EvaluatorException("No elements in pair")
        case Seq(_)                 ⇒ throw new EvaluatorException("Only one element in pair")
        case Seq(first, second, _*) ⇒ ToStringifier.stringify(first) -> second
      }
    case obj: MashObject ⇒
      obj.immutableFields.toSeq match {
        case Seq()                            ⇒ throw new EvaluatorException("No elements in pair")
        case Seq(_)                           ⇒ throw new EvaluatorException("Only one element in pair")
        case Seq((_, first), (_, second), _*) ⇒ ToStringifier.stringify(first) -> second
      }
    case _ ⇒ throw new EvaluatorException(s"Invalid pair of type ${value.typeName}")
  }

  override def summaryOpt = Some("Construct an object from a list of pairs")

}
