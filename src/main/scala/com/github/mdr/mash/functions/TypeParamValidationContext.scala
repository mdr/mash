package com.github.mdr.mash.functions

import com.github.mdr.mash.inference.{ ValueInfo, Type, TypedArgument, TypedArguments }
import com.github.mdr.mash.ns.core.BooleanClass

import scala.PartialFunction.cond

class TypeParamValidationContext(params: ParameterModel, arguments: TypedArguments) {

  private var boundArguments: Map[String, ValueInfo] = Map()
  private var boundNames: Map[String, Type] = Map()
  private var posToParam: Map[Int, Parameter] = Map()
  private var lastParameterConsumed = false

  def bind(): BoundTypeParams = {
    handleLastArg()
    handlePositionalArgs()
    handleFlagArgs()
    BoundTypeParams(boundArguments, boundNames, posToParam)
  }

  private def handleLastArg() {
    for {
      lastParam ← params.lastParamOpt
      paramName = lastParam.name
      lastArg ← arguments.positionArgs.lastOption
      if !arguments.isProvidedAsNamedArg(paramName)
    } {
      lastParameterConsumed = true
      boundArguments += lastParam.name -> lastArg
      lastArg.typeOpt.foreach {
        boundNames += lastParam.name -> _
      }
      posToParam += posOfArg(lastArg) -> lastParam
    }
  }

  private def handlePositionalArgs() = {
    val regularPosParams = params.positionalParams.filterNot(p ⇒ p.isVariadic || p.isLast)
    val positionArgs = if (lastParameterConsumed) arguments.positionArgs.init else arguments.positionArgs

    if (positionArgs.size > regularPosParams.size)
      for (variadicParam ← params.variadicParamOpt) {
        val varargs = positionArgs.drop(regularPosParams.size)
        val varargType = varargs.flatMap(_.typeOpt).headOption.getOrElse(Type.Any).seq
        boundArguments += variadicParam.name -> ValueInfo(None, Some(varargType))
        boundNames += variadicParam.name -> varargType
        val extraArgs = positionArgs.drop(regularPosParams.size)
        for (arg ← extraArgs)
          posToParam += posOfArg(arg) -> variadicParam
      }

    for ((param, arg) ← regularPosParams zip positionArgs) {
      param.patternOpt match {
        case Some(ParamPattern.Object(fieldNames)) ⇒
          val fieldTypes: Map[String, Type] = arg.typeOpt.map {
            case Type.Object(knownFields) ⇒ knownFields
            case Type.Instance(klass)     ⇒ klass.fieldsMap.mapValues(_.fieldType)
            case _                        ⇒ Map[String, Type]()
          }.getOrElse(Map())
          for (fieldName ← fieldNames)
            boundNames += fieldName -> fieldTypes.getOrElse(fieldName, Type.Any)
        case Some(ParamPattern.Hole)               ⇒

        case None =>
          boundArguments += param.name -> arg
          arg.typeOpt.foreach {
            boundNames += param.name -> _
          }
      }
      posToParam += posOfArg(arg) -> param
    }
  }

  private def posOfArg(arg: ValueInfo): Int =
    arguments.arguments.indexWhere(cond(_) { case TypedArgument.PositionArg(`arg`) ⇒ true })

  private def handleFlagArgs() {
    for (flagArg ← arguments.argSet)
      bindFlagParam(flagArg, arg = ValueInfo(None, Some(BooleanClass)))
    for {
      (flagArg, valueOpt) ← arguments.argValues
      value ← valueOpt
    } bindFlagParam(flagArg, arg = value)
  }

  private def bindFlagParam(paramName: String, arg: ValueInfo) =
    for (param ← params.paramByName.get(paramName)) {
      boundArguments += param.name -> arg
      arg.typeOpt.foreach {
        boundNames += param.name -> _
      }
      val argIndex = arguments.arguments.indexWhere {
        case TypedArgument.LongFlag(`paramName`, _) ⇒ true
        case TypedArgument.ShortFlag(flags)         ⇒ flags contains paramName
        case _                                      ⇒ false
      }
      posToParam += argIndex -> param
    }

}