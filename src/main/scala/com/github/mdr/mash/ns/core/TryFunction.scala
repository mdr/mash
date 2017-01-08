package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.{ Arguments, EvaluationInterruptedException }
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.MashValue

object TryFunction extends MashFunction("core.try") {

  object Params {
    val Body = Parameter(
      name = "body",
      summary = "Code to execute",
      isLazy = true)
    val Catch = Parameter(
      name = "catch",
      summary = "Code to execute if an exception is thrown in the body",
      isLazy = true)
  }

  import Params._

  val params = ParameterModel(Seq(Body, Catch))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val body = boundParams(Body).asInstanceOf[MashFunction]
    val catchBlock = boundParams(Catch).asInstanceOf[MashFunction]
    try
      body.apply(Arguments(Seq()))
    catch {
      case e: EvaluationInterruptedException ⇒ throw e
      case _: Throwable ⇒ catchBlock.apply(Arguments(Seq()))
    }
  }

  override def typeInferenceStrategy = new TypeInferenceStrategy {

    def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] =
      params.bindTypes(arguments).getType(Body)

  }

  override def summary = "Execute the given code, catching any exceptions"

}