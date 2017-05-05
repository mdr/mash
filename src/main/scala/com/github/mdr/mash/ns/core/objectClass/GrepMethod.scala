package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.collections.GrepFunction
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

object GrepMethod extends MashMethod("grep") {

  import GrepFunction.Params._

  val params = ParameterModel(Seq(Query, IgnoreCase, Regex, Negate))

  def call(target: MashValue, boundParams: BoundParams): MashObject = {
    val obj = target.asInstanceOf[MashObject]
    doGrep(obj, boundParams)
  }

  def doGrep(obj: MashObject, boundParams: BoundParams): MashObject = {
    val ignoreCase = boundParams(IgnoreCase).isTruthy
    val regex = boundParams(Regex).isTruthy
    val query = ToStringifier.stringify(boundParams(Query))
    val negate = boundParams(Negate).isTruthy
    val items = obj.immutableFields.map(MashObject.of(_)).toSeq
    val filteredItems = GrepFunction.runGrep(items, query, ignoreCase, regex, negate)
    filteredItems.elements.flatMap(_.asObject).fold(MashObject.empty)(_ + _)
  }

  override def summaryOpt: Option[String] = Some("Find all the fields in the object which match the given query somewhere in its String representation")

  override def typeInferenceStrategy = ObjectClass

  override val isShy = true

}
