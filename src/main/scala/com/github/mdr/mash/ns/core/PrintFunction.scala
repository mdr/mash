package com.github.mdr.mash.ns.core

import java.io.PrintStream

import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference._
import com.github.mdr.mash.runtime.MashUnit

object PrintFunction extends MashFunction("core.print") {

  private val output: PrintStream = System.out

  object Params {
    val Item = Parameter(
      nameOpt = Some("item"),
      summaryOpt = Some("Item to negate"))
  }
  import Params._

  val params = ParameterModel(Seq(Item))

  def apply(arguments: Arguments): MashUnit = {
    val boundParams = params.validate(arguments)
    val item = boundParams(Item)
    output.println(ToStringifier.stringify(item))
    MashUnit
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Unit)

  override def summaryOpt = Some("Print the given argument to standard output")

}