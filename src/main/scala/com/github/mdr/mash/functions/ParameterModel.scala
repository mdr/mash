package com.github.mdr.mash.functions

import com.github.mdr.mash.compiler.DesugarHoles
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.inference.TypedArguments

case class ParameterModel(params: Seq[Parameter] = Seq()) {

  require(params.count(_.isVariadic) <= 1)
  require(params.count(_.isLast) <= 1)

  val lastParamOpt: Option[Parameter] = params.find(_.isLast)

  val variadicParamOpt: Option[Parameter] = params.find(_.isVariadic)

  val positionalParams = params.filterNot(p ⇒ p.isFlag || p.isNamedArgsParam)

  // Lookup parameter by name or short flag (if it has one)
  val paramByName: Map[String, Parameter] = {
    var paramMap: Map[String, Parameter] = Map()
    for (param ← params) {
      for (paramName <- param.nameOpt)
        paramMap += paramName -> param
      for (shortFlag ← param.shortFlagOpt)
        paramMap += shortFlag.toString -> param
    }
    paramMap
  }

  def bindTo(arguments: Arguments): BoundParams =
    new ParamBindingContext(this, arguments).bind

  def flags: Seq[Flag] = params.map(param ⇒
    Flag(param.summaryOpt orElse param.nameOpt, param.shortFlagOpt.map(_.toString), param.nameOpt))

  def allowsNullary: Boolean = params.forall(allowsNullary(_))

  private def allowsNullary(p: Parameter): Boolean =
    (p.isVariadic && !p.variadicAtLeastOne) || p.defaultValueGeneratorOpt.isDefined || p.isNamedArgsParam

  def allowsBinary: Boolean = params.exists(_.isVariadic) || positionalParams.size >= 2

  def callingSyntax: String = {
    val positionalParams =
      for (param ← params.filterNot(_.isFlag)) yield {
        val paramName = param.nameOpt getOrElse Parameter.AnonymousParamName
        val name = if (paramName == DesugarHoles.VariableName) Parameter.AnonymousParamName else paramName
        if (param.isVariadic)
          if (param.variadicAtLeastOne)
            s"<$name>+..."
          else
            s"<$name>..."
        else if (param.isOptional)
          s"[<$name>]"
        else
          s"<$name>"
      }
    val flagParams =
      for (param ← params.filter(_.isFlag)) yield {
        val name = param.nameOpt getOrElse Parameter.AnonymousParamName
        val flagValueName = param.flagValueNameOpt.getOrElse("value")
        val flagValueSuffix =
          if (param.isBooleanFlag)
            ""
          else if (param.isFlagValueMandatory)
            s"=<$flagValueName>"
          else
            s"[=<$flagValueName>]"
        val longForm = s"--$name$flagValueSuffix"
        val main = param.shortFlagOpt match {
          case Some(shortFlag) ⇒ s"$longForm | -$shortFlag"
          case None            ⇒ longForm
        }
        if (param.shortFlagOpt.isDefined)
          s"($main)"
        else
          main
      }
    (flagParams ++ positionalParams).mkString(" ")
  }

  def bindTypes(arguments: TypedArguments): BoundTypeParams =
    new TypeParamBindingContext(this, arguments).bind()

}

