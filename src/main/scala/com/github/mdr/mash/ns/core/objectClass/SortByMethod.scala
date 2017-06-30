package com.github.mdr.mash.ns.core.objectClass

import com.github.mdr.mash.functions.BoundParams.Function1Or2
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.ns.collections.SortFunction.NaturalMashValueOrdering
import com.github.mdr.mash.ns.collections.{ SortByFunction, SortFunction }
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.runtime.{ MashObject, MashValue, MashValueOrdering }
import com.github.mdr.mash.utils.Utils._

object SortByMethod extends MashMethod("sortBy") {

  import SortByFunction.Params._
  import SortFunction.Params._

  val params = ParameterModel(Descending, NaturalOrder, Attributes)

  def call(target: MashValue, boundParams: BoundParams): MashObject = {
    val obj = target.asInstanceOf[MashObject]
    val descending = boundParams(Descending).isTruthy
    val naturalOrder = boundParams(NaturalOrder).isTruthy
    val attributes = validateAttributes(boundParams)
    sort(obj, attributes, descending, naturalOrder)
  }

  private def sort(obj: MashObject,
                   attributes: Seq[Function1Or2],
                   descending: Boolean,
                   naturalOrder: Boolean): MashObject = {
    val ordering = if (naturalOrder) NaturalMashValueOrdering else MashValueOrdering
    val sorted = obj.immutableFields.toSeq.sortWith { (fv1, fv2) ⇒
      val attrs1 = attributes.map(getAttributeFrom(fv1)).toList
      val attrs2 = attributes.map(getAttributeFrom(fv2)).toList
      MashValueOrdering.compareLists(attrs1, attrs2, ordering) < 0
    }
    val finalSequence = sorted.when(descending, _.reverse)
    MashObject.of(finalSequence)
  }

  private def getAttributeFrom(fv: (MashValue, MashValue))(attribute: Function1Or2): MashValue =
    attribute match {
      case Left(f)  ⇒ f(fv._1)
      case Right(f) ⇒ f.tupled(fv)
    }

  private def validateAttributes(boundParams: BoundParams): Seq[Function1Or2] = {
    boundParams.validateSequence(Attributes, allowStrings = false)
      .map(boundParams.validateFunction1Or2(_, Attributes))
  }

  override def typeInferenceStrategy = ObjectClass

  override def summaryOpt: Option[String] = Some("Return a new object with fields sorted according to a given attribute")

  override val isShy = true

}