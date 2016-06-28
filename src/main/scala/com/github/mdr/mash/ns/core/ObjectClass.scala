package com.github.mdr.mash.ns.core

import scala.collection.immutable.ListMap

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.ns.core.FieldAndValueClass.Fields
import com.github.mdr.mash.runtime._

object ObjectClass extends MashClass("core.Object") {

  override val methods = Seq(
    FieldsMethod)

  object FieldsMethod extends MashMethod("fields") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = target match {
      case MashObject(fields, _) ⇒
        MashList(fields.toSeq.map {
          case (name, value) ⇒
            import FieldAndValueClass.Fields._
            MashObject(ListMap(
              Name -> MashString(name),
              Value -> value), FieldAndValueClass)
        })
      case _ ⇒
        MashList(Seq())
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Seq(FieldAndValueClass))

    override def summary = "Return the fields of this object"
  }

  override def summary = "The class of all objects"

  override def parentOpt = Some(AnyClass)

}