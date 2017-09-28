package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.collections.GrepFunction
import com.github.mdr.mash.ns.collections.GrepFunction.GrepOptions
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.runtime.{ MashObject, MashValue }

object GrepMethod extends MashMethod("grep") {

  import GrepFunction.Params._

  val params = ParameterModel(Query, IgnoreCase, Regex, Negate, First)

  def call(target: MashValue, boundParams: BoundParams): MashValue = {
    val obj = target.asInstanceOf[MashObject]
    doGrep(obj, boundParams)
  }

  def doGrep(obj: MashObject, boundParams: BoundParams): MashValue = {
    val ignoreCase = boundParams(IgnoreCase).isTruthy
    val regex = boundParams(Regex).isTruthy
    val query = ToStringifier.stringify(boundParams(Query))
    val negate = boundParams(Negate).isTruthy
    val first = boundParams(First).isTruthy
    val items = obj.immutableFields.toSeq.map(MashObject.of(_))
    val options = GrepOptions(ignoreCase, regex, negate, first, ignoreFields = false)
    if (first) {
      GrepFunction.grepForFirst(items, query, options)
    } else {
      val filteredItems = GrepFunction.grepForAll(items, query, options)
      filteredItems.elements.flatMap(_.asObject).fold(MashObject.empty)(_ ++ _)
    }
  }

  override def summaryOpt: Option[String] = Some("Find all the fields in the object which match the given query somewhere in its String representation")

  override def typeInferenceStrategy = ObjectClass

  override val isShy = true

}
