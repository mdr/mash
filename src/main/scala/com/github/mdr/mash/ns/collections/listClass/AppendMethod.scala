package com.github.mdr.mash.ns.collections.listClass

import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ Inferencer, MethodTypeInferenceStrategy, Type, TypedArguments }
import com.github.mdr.mash.runtime.{ MashList, MashValue }

object AppendMethod extends MashMethod("append") {

  object Params {
    val Item = Parameter(
      nameOpt = Some("item"),
      summaryOpt = Some("An item to place at the end of the List"))
  }

  import Params._

  override val params = ParameterModel(Item)

  def call(target: MashValue, boundParams: BoundParams): MashList =
    MashList(target.asInstanceOf[MashList].immutableElements :+ boundParams(Item))

  override def summaryOpt: Option[String] = Some("Append the given item to the end of this List")

  object AppendMethodTypeInferenceStrategy extends MethodTypeInferenceStrategy {

    def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
      val argBindings = params.bindTypes(arguments)
      targetTypeOpt orElse argBindings.getArgument(Item).flatMap(_.typeOpt).map(_.seq)
    }

  }

  override def typeInferenceStrategy = AppendMethodTypeInferenceStrategy

}
