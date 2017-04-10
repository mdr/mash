package com.github.mdr.mash.ns.collections.listClass

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ Inferencer, Type, TypeInferenceStrategy, TypedArguments }
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.runtime.MashList

object NewStaticMethod extends MashFunction("new") {

  object Params {
    val Elements = Parameter(
      nameOpt = Some("elements"),
      summaryOpt = Some("Elements of the list"),
      isVariadic = true)
  }

  import Params._

  override val params = ParameterModel(Seq(Elements))

  def apply(boundParams: BoundParams): MashList = {
    MashList(boundParams.validateSequence(Elements))
  }

  override def summaryOpt: Option[String] = Some("Construct a new List with the given elements")

  override object typeInferenceStrategy extends TypeInferenceStrategy {

    def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
      val argBindings = params.bindTypes(arguments)
      argBindings.getType(Elements) orElse Some(Seq(AnyClass))
    }

  }

}
