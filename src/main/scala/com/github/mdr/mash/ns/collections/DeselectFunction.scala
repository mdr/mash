package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.objectClass.GetMethod
import com.github.mdr.mash.runtime._

object DeselectFunction extends MashFunction("collections.deselect") {

  object Params {
    val Fields = Parameter(
      nameOpt = Some("fields"),
      summaryOpt = Some("Fields from the object"),
      isVariadic = true,
      variadicAtLeastOne = true)
    val Target = Parameter(
      nameOpt = Some("target"),
      summaryOpt = Some("Object or sequence of objects to remove fields from"))
  }

  import Params._

  val params = ParameterModel(Seq(Fields, Target))

  def call(boundParams: BoundParams): MashValue = {
    val fields: Seq[String] = boundParams.validateSequence(Fields).collect {
      case s: MashString ⇒ s.s
      case field         ⇒ boundParams.throwInvalidArgument(Fields, "Invalid field name of type: " + field.typeName)
    }
    boundParams(Target) match {
      case xs: MashList ⇒ xs.map(doDeselect(_, fields))
      case x            ⇒ doDeselect(x, fields)
    }
  }

  private def doDeselect(value: MashValue, fields: Seq[String]): MashValue = value match {
    case obj: MashObject ⇒ MashObject.of(obj.fields.filterNot(fields contains _._1))
    case _               ⇒ value
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = {
    val argBindings = params.bindTypes(arguments)
    val completionSpecOpt =
      for {
        param ← argBindings.paramAt(argPos)
        if param == Fields
        targetType <- argBindings.getType(Target)
        actualTargetType = targetType match {
          case Type.Seq(elemType) ⇒ elemType
          case _                  ⇒ targetType
        }
      } yield CompletionSpec.Items(GetMethod.getFields(actualTargetType))
    completionSpecOpt.toSeq
  }

  override def summaryOpt = Some("Remove fields from an object or sequence of objects")

}
