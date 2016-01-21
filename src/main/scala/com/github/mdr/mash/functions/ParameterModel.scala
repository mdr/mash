package com.github.mdr.mash.functions

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.inference.AnnotatedExpr
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.TypedArgument
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.core.BooleanClass
import com.github.mdr.mash.parser.AbstractSyntax.Argument
import com.github.mdr.mash.evaluator.EvaluatedArgument

case class ParameterModel(params: Seq[Parameter] = Seq()) {

  val lastParamOpt: Option[Parameter] = params.find(_.isLast)

  val variadicParamOpt: Option[Parameter] = params.find(_.isVariadic)

  val (flagParams, positionalParams) = params.partition(_.isFlag)

  // Lookup parameter by name (or short flag)
  val paramByName: Map[String, Parameter] = {
    var paramMap: Map[String, Parameter] = Map()
    for (param ← params) {
      paramMap += param.name -> param
      for (shortFlag ← param.shortFlagOpt)
        paramMap += shortFlag.toString -> param
    }
    paramMap
  }

  def validate(arguments: Arguments, ignoreAdditionalParameters: Boolean = false): BoundParams =
    new ParamValidationContext(this, arguments, ignoreAdditionalParameters).validate()

  def flags: Seq[Flag] = params.map(param ⇒ Flag(param.summary, param.shortFlagOpt.map(_.toString), Some(param.name)))

  def allowsNullary: Boolean = params.forall(p ⇒ p.isVariadic || p.defaultValueGeneratorOpt.isDefined)

  def callingSyntax: String = {
    val positionalParams =
      for (param ← params.filterNot(_.isFlag)) yield {
        val name = param.name
        if (param.isVariadic)
          s"<$name>..."
        else if (param.isOptional)
          s"[<$name>]"
        else
          s"<$name>"
      }
    val flagParams =
      for (param ← params.filter(_.isFlag)) yield {
        val name = param.name
        val flagValueName = param.flagValueNameOpt.getOrElse("value")
        val flagValueSuffix =
          if (param.isFlagValueAllowed)
            if (param.isFlagValueMandatory)
              s"=<$flagValueName>"
            else
              s"[=<$flagValueName>]"
          else
            ""
        val longForm = s"--$name$flagValueSuffix"
        val main = param.shortFlagOpt match {
          case Some(shortFlag) ⇒ s"$longForm | -$shortFlag"
          case None            ⇒ longForm
        }
        if (param.isOptional)
          s"[$main]"
        if (param.shortFlagOpt.isDefined)
          s"($main)"
        else
          main
      }
    (flagParams ++ positionalParams).mkString(" ")
  }

  def bindTypes(arguments: TypedArguments): BoundTypeParams =
    BoundTypeParams(new TypeParamValidationContext(arguments).bind().boundParams)

  private class TypeParamValidationContext(arguments: TypedArguments) {

    private var boundParams: Map[String, AnnotatedExpr] = Map()
    private var posToName: Map[Int, String] = Map()
    private var lastParameterConsumed = false

    def bind(): TypedBoundParameters = {
      handleLastArg()
      handlePositionalArgs()
      handleFlagArgs()
      TypedBoundParameters(boundParams, posToName)
    }

    private def handleLastArg() {
      for (lastParam ← lastParamOpt)
        for (lastArg ← arguments.positionArgs.lastOption) {
          boundParams += lastParam.name -> lastArg
          posToName += arguments.arguments.indexOf(lastArg) -> lastParam.name
          lastParameterConsumed = true
        }
    }

    private def handlePositionalArgs() = {
      val regularPosParams = positionalParams.filterNot(p ⇒ p.isVariadic || p.isLast)
      val positionArgs = if (lastParameterConsumed) arguments.positionArgs.init else arguments.positionArgs

      if (positionArgs.size > regularPosParams.size)
        for (variadicParam ← variadicParamOpt) {
          val varargs = positionArgs.drop(regularPosParams.size)
          val typ = Type.Seq(varargs.flatMap(_.typeOpt).headOption.getOrElse(Type.Any))
          boundParams += variadicParam.name -> AnnotatedExpr(None, Some(typ))
          for (pos ← regularPosParams.size to positionArgs.size - 1)
            posToName += arguments.arguments.indexOf(pos) -> variadicParam.name
        }

      for ((param, arg) ← regularPosParams zip positionArgs) {
        boundParams += param.name -> arg
        posToName += arguments.arguments.indexOf(arg) -> param.name
      }
    }

    private def handleFlagArgs() {
      for (paramName ← arguments.argSet)
        bindFlagParam(paramName, expr = AnnotatedExpr(None, Some(Type.Instance(BooleanClass))))
      for ((paramName, valueOpt) ← arguments.argValues; value ← valueOpt)
        bindFlagParam(paramName, expr = value)
    }

    private def bindFlagParam(paramName: String, expr: AnnotatedExpr) =
      for (param ← paramByName.get(paramName)) {
        boundParams += param.name -> expr
        val argIndex = arguments.arguments.indexWhere {
          case TypedArgument.LongFlag(`paramName`, _) ⇒ true
          case TypedArgument.ShortFlag(flags)         ⇒ flags.contains(paramName)
          case _                                      ⇒ false
        }
        posToName += argIndex -> param.name
      }

  }

}

case class BoundTypeParams(params: Map[String, AnnotatedExpr]) {

  def apply(param: String): AnnotatedExpr = params(param)

  def apply(param: Parameter): AnnotatedExpr = params(param.name)

  def get(param: Parameter): Option[AnnotatedExpr] = params.get(param.name)

  def get(param: String): Option[AnnotatedExpr] = params.get(param)

  def contains(param: String) = get(param).isDefined

  def contains(param: Parameter) = get(param).isDefined

}

case class TypedBoundParameters(boundParams: Map[String, AnnotatedExpr], posToName: Map[Int, String])