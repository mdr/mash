package com.github.mdr.mash.ns.core.help

import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.{ ClassClass, FunctionClass }
import com.github.mdr.mash.runtime._

object HelpFunction extends MashFunction("core.help.help") {

  object Params {
    val Item = Parameter(
      nameOpt = Some("item"),
      summaryOpt = Some("The item to find help for"),
      isSafe = true)
  }

  import Params._

  val params = ParameterModel(Item)

  def call(boundParams: BoundParams): MashValue = {
    val item = boundParams(Item)
    HelpCreator.getHelp(item)
  }

  override def typeInferenceStrategy = HelpTypeInferenceStrategy

  override def summaryOpt = Some("Find help for the given function, method, field or class")

}

object HelpTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    val argBindings = HelpFunction.params.bindTypes(arguments)
    import HelpFunction.Params._
    argBindings.getType(Item).collect {
      case Type.BuiltinFunction(_) | _: Type.UserDefinedFunction          ⇒ FunctionClass
      case Type.BoundBuiltinMethod(_, _) | _: Type.BoundUserDefinedMethod ⇒ MethodHelpClass
      case Type.Instance(ClassClass) | _: Type.UserClass                  ⇒ ClassHelpClass
    }

  }

}