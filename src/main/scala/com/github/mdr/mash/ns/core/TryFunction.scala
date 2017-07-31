package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.EvaluationInterruptedException
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.core.NoArgFunction.NoArgValue
import com.github.mdr.mash.runtime.{ MashUnit, MashValue }

import scala.util.control.NonFatal

object TryFunction extends MashFunction("core.try") {

  object Params {
    val Body = Parameter(
      nameOpt = Some("body"),
      summaryOpt = Some("Code to execute"),
      isLazy = true)
    val Catch = Parameter(
      nameOpt = Some("catch"),
      summaryOpt = Some("Code to execute if an exception is thrown in the body"),
      defaultValueGeneratorOpt = Some(NoArgValue),
      isLazy = true)
    val Finally = Parameter(
      nameOpt = Some("finally"),
      summaryOpt = Some("Code to execute after execution of the body, regardless of whether an exception is thrown or not"),
      defaultValueGeneratorOpt = Some(NoArgValue),
      isLazy = true)
  }

  import Params._

  val params = ParameterModel(Body, Catch, Finally)

  def call(boundParams: BoundParams): MashValue = {
    val body = boundParams(Body).asInstanceOf[MashFunction]
    try
      body.callNullary()
    catch {
      case EvaluationInterruptedException ⇒ throw EvaluationInterruptedException
      case NonFatal(_)                    ⇒
        NoArgFunction.option(boundParams(Catch)) match {
          case Some(catchBlock) ⇒ catchBlock.asInstanceOf[MashFunction].callNullary()
          case None             ⇒ MashUnit
        }

    } finally
      for (finallyBlock ← NoArgFunction.option(boundParams(Finally)))
        finallyBlock.asInstanceOf[MashFunction].callNullary()
  }

  override def typeInferenceStrategy = new TypeInferenceStrategy {

    def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] =
      params.bindTypes(arguments).getType(Body)

  }

  override def summaryOpt = Some("Execute the given code, catching any exceptions")

}