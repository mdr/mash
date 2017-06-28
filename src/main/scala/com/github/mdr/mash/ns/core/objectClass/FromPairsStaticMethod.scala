package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.NoArgFunction.NoArgValue
import com.github.mdr.mash.runtime.{ MashList, MashNumber, MashObject, MashValue }

object FromPairsStaticMethod extends MashFunction("fromPairs") {

  object Params {
    val FieldSelector = Parameter(
      nameOpt = Some("fieldSelector"),
      summaryOpt = Some("How to select the fields from the given items"),
      defaultValueGeneratorOpt = Some(() ⇒ NoArgValue))
    val ValueSelector = Parameter(
      nameOpt = Some("valueSelector"),
      summaryOpt = Some("How to select the values from the given items"),
      defaultValueGeneratorOpt = Some(() ⇒ NoArgValue))
    val Pairs = Parameter(
      nameOpt = Some("pairs"),
      summaryOpt = Some("Pairs to merge into an object"))
  }

  import Params._

  val params = ParameterModel(FieldSelector, ValueSelector, Pairs)

  sealed trait Selector {
    def select(xs: MashValue): MashValue
  }

  case class IndexSelector(i: Int) extends Selector {

    def select(value: MashValue): MashValue = value match {
      case obj: MashObject ⇒ select(obj.immutableFields.toSeq.map(_._2), "Object")
      case xs: MashList    ⇒ select(xs.immutableElements, "List")
      case _               ⇒ throw new EvaluatorException(s"Invalid pair of type ${value.typeName}")
    }

    private def select(elements: Seq[MashValue], collection: String): MashValue = {
      if (elements.isEmpty)
        throw new EvaluatorException(s"No elements in $collection, at least two are required")
      else if (elements.size == 1)
        throw new EvaluatorException("Only one element in $collection, at least two are required")
      else if (elements.size <= i)
        throw new EvaluatorException(s"Cannot select element $i from $collection of length ${elements.size}")
      else
        elements(i)
    }

  }

  case class FunctionSelector(f: MashValue ⇒ MashValue) extends Selector {

    def select(value: MashValue): MashValue = f(value)

  }

  override def call(boundParams: BoundParams): MashObject = {
    val items = boundParams.validateSequence(Pairs, allowStrings = false)
    val fieldSelector = validateSelector(boundParams, FieldSelector, defaultIndex = 0)
    val valueSelector = validateSelector(boundParams, ValueSelector, defaultIndex = 1)
    val pairs =
      for {
        item ← items
        field = fieldSelector.select(item)
        value = valueSelector.select(item)
      } yield field -> value
    MashObject.of(pairs)
  }

  private def validateSelector(boundParams: BoundParams, param: Parameter, defaultIndex: Int): Selector =
    boundParams(param) match {
      case NoArgValue    ⇒ IndexSelector(defaultIndex)
      case n: MashNumber ⇒ IndexSelector(boundParams.validateNonNegativeInteger(param))
      case _             ⇒ FunctionSelector(boundParams.validateFunction(param))
    }

  override def summaryOpt = Some("Construct an Object from a List of pairs")

}
