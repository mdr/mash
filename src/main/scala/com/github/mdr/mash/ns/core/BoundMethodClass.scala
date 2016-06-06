package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.time._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.ns.core.help.FunctionHelpClass
import com.github.mdr.mash.ns.core.help.ParameterHelpClass
import com.github.mdr.mash.functions.Parameter
import scala.collection.immutable.ListMap
import com.github.mdr.mash.ns.core.help.HelpFunction
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashValue

object BoundMethodClass extends MashClass("core.BoundMethod") {

  override val methods = Seq(
    HelpMethod,
    TargetMethod)

  object HelpMethod extends MashMethod("help") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashObject = {
      params.validate(arguments)
      HelpFunction.getHelp(target.asInstanceOf[BoundMethod])
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(FunctionHelpClass)

    override def summary = "Help documentation for this method"

  }

  object TargetMethod extends MashMethod("target") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      target.asInstanceOf[BoundMethod].target
    }

    override def typeInferenceStrategy = new MethodTypeInferenceStrategy {

      def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] =
        targetTypeOpt.collect {
          case Type.BoundMethod(receiver, _) ⇒ receiver
        }

    }

    override def summary = "Target of this bound method"

    override def descriptionOpt = Some("""Examples:
[1, 2, 3].sortBy.target # [1, 2, 3]""")

  }

  override def summary = "A method bound to a target"

}