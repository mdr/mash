package com.github.mdr.mash.ns.core

import scala.collection.immutable.ListMap
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.ns.core.FieldAndValueClass.Fields
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.functions.Parameter

object ObjectClass extends MashClass("core.Object") {

  override val methods = Seq(
    FieldsMethod,
    HasFieldMethod)

  object FieldsMethod extends MashMethod("fields") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = {
      val obj = target.asInstanceOf[MashObject]
      def asObject(name: String, value: MashValue) = {
        import FieldAndValueClass.Fields._
        MashObject.of(ListMap(
          Name -> MashString(name),
          Value -> value), FieldAndValueClass)
      }
      MashList(obj.fields.toSeq.map((asObject _).tupled))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Seq(FieldAndValueClass))

    override def summary = "Return the fields of this object"
  }


  object HasFieldMethod extends MashMethod("hasField") {

    object Params {
      val Name = Parameter(
        name = "name",
        summary = "Field name")
    }
    import Params._

    val params = ParameterModel(Seq(Name))

    def apply(target: MashValue, arguments: Arguments): MashBoolean = {
      val obj = target.asInstanceOf[MashObject]
      val boundParams = params.validate(arguments)
      val field = boundParams.validateString(Name).s
      MashBoolean(obj.fields.contains(field))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(BooleanClass)

    override def summary = "Return true if this object contains the given field"
  }

  override def summary = "The class of all objects"

  override def parentOpt = Some(AnyClass)

}