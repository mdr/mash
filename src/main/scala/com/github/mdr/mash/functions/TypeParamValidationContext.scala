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
import scala.PartialFunction.cond

class TypeParamValidationContext(params: ParameterModel, arguments: TypedArguments) {

  private var boundParams: Map[String, AnnotatedExpr] = Map()
  private var posToParam: Map[Int, Parameter] = Map()
  private var lastParameterConsumed = false

  def bind(): BoundTypeParams = {
    handleLastArg()
    handlePositionalArgs()
    handleFlagArgs()
    BoundTypeParams(boundParams, posToParam)
  }

  private def handleLastArg() {
    for {
      lastParam ← params.lastParamOpt
      paramName = lastParam.name
      lastArg ← arguments.positionArgs.lastOption
      if !arguments.isProvidedAsNamedArg(paramName)
    } {
      lastParameterConsumed = true
      boundParams += lastParam.name -> lastArg
      posToParam += posOfArg(lastArg) -> lastParam
    }
  }

  private def handlePositionalArgs() = {
    val regularPosParams = params.positionalParams.filterNot(p ⇒ p.isVariadic || p.isLast)
    val positionArgs = if (lastParameterConsumed) arguments.positionArgs.init else arguments.positionArgs

    if (positionArgs.size > regularPosParams.size)
      for (variadicParam ← params.variadicParamOpt) {
        val varargs = positionArgs.drop(regularPosParams.size)
        val varargType = varargs.flatMap(_.typeOpt).headOption.getOrElse(Type.Any)
        boundParams += variadicParam.name -> AnnotatedExpr(None, Some(Type.Seq(varargType)))
        val extraArgs = positionArgs.drop(regularPosParams.size)
        for (arg ← extraArgs)
          posToParam += posOfArg(arg) -> variadicParam
      }

    for ((param, arg) ← regularPosParams zip positionArgs) {
      boundParams += param.name -> arg
      posToParam += posOfArg(arg) -> param
    }
  }

  private def posOfArg(arg: AnnotatedExpr): Int =
    arguments.arguments.indexWhere(cond(_) { case TypedArgument.PositionArg(`arg`) ⇒ true })

  private def handleFlagArgs() {
    for (paramName ← arguments.argSet)
      bindFlagParam(paramName, expr = AnnotatedExpr(None, Some(Type.Instance(BooleanClass))))
    for ((paramName, valueOpt) ← arguments.argValues; value ← valueOpt)
      bindFlagParam(paramName, expr = value)
  }

  private def bindFlagParam(paramName: String, expr: AnnotatedExpr) =
    for (param ← params.paramByName.get(paramName)) {
      boundParams += param.name -> expr
      val argIndex = arguments.arguments.indexWhere {
        case TypedArgument.LongFlag(`paramName`, _) ⇒ true
        case TypedArgument.ShortFlag(flags)         ⇒ flags.contains(paramName)
        case _                                      ⇒ false
      }
      posToParam += argIndex -> param
    }

}