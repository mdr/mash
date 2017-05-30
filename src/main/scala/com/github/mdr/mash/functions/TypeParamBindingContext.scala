package com.github.mdr.mash.functions

import com.github.mdr.mash.inference._

object TypeParamBindingContext {

  def bindPatternParam(pattern: ParamPattern, typeOpt: Option[Type]): Map[String, Type] =
    pattern match {
      case ParamPattern.Object(entries)   ⇒
        val fieldTypes: Map[String, Type] = typeOpt.map {
          case Type.Object(knownFields) ⇒ knownFields
          case Type.Instance(klass)     ⇒ klass.fieldsMap.mapValues(_.fieldType)
          case _                        ⇒ Map[String, Type]()
        }.getOrElse(Map())
        def inferEntry(entry: ParamPattern.ObjectEntry): Map[String, Type] =
          entry match {
            case ParamPattern.ObjectEntry(fieldName, None)               ⇒
              Map(fieldName -> fieldTypes.getOrElse(fieldName, Type.Any))
            case ParamPattern.ObjectEntry(fieldName, Some(valuePattern)) ⇒
              bindPatternParam(valuePattern, fieldTypes.get(fieldName))
          }
        entries.flatMap(inferEntry).toMap
      case ParamPattern.Hole              ⇒
        Map()
      case ParamPattern.Ident(identifier) ⇒
        Map(identifier -> typeOpt.getOrElse(Type.Any))
      case ParamPattern.List(patterns)    ⇒
        val elementTypeOpt = typeOpt.collect { case Type.Seq(elementType) ⇒ elementType }
        patterns.flatMap(bindPatternParam(_, elementTypeOpt)).toMap
    }

  def bindParam(param: Parameter, argTypeOpt: Option[Type]): Map[String, Type] =
    param.patternOpt match {
      case Some(pattern) ⇒
        bindPatternParam(pattern, argTypeOpt)
      case None          ⇒
        (for {
          paramName ← param.nameOpt
        } yield paramName -> argTypeOpt.getOrElse(Type.Any)).toMap
    }

}

class TypeParamBindingContext(params: ParameterModel, arguments: TypedArguments) {

  import TypeParamBindingContext._

  private var boundArguments: Map[Parameter, ValueInfo] = Map()
  private var boundNames: Map[String, Type] = Map()

  def bind(): BoundTypeParams = {
    val result = new GeneralArgBinder(params, generalArguments, forgiving = true).bind
    bindParams(result.parameterToArguments)
    BoundTypeParams(boundArguments, boundNames, result.posToParam)
  }

  private def bindParams(parameterToArgs: Map[Parameter, Seq[TypedArgument]]) =
    for ((param, evalArgs) ← parameterToArgs)
      if (param.isVariadic)
        bindVariadicParam(param, evalArgs)
      else
        bindRegularParam(param, evalArgs)

  private def bindRegularParam(param: Parameter, evalArgs: Seq[TypedArgument]) =
    for {
      arg ← evalArgs.headOption
      value ← arg.valueOpt
    } {
      val argTypeOpt = value.typeOpt
      val paramNames = bindParam(param, argTypeOpt)
      boundArguments += param -> value
      boundNames ++= paramNames
    }

  private def bindVariadicParam(param: Parameter, evalArgs: Seq[TypedArgument]) = {
    val individualArgType = evalArgs.flatMap(_.valueOpt).flatMap(_.typeOpt).headOption.getOrElse(Type.Any)
    val varargType = individualArgType match {
      case Type.Seq(elementType) if param.variadicFlatten ⇒ elementType.seq
      case t                                              ⇒ t.seq
    }
    boundArguments += param -> ValueInfo(None, Some(varargType))
    param.nameOpt.foreach(boundNames += _ -> varargType)
  }

  private def generalArguments: Seq[GeneralArgument[TypedArgument]] =
    arguments.arguments.map {
      case arg@TypedArgument.ShortFlag(flags)  ⇒ GeneralArgument.ShortFlag(flags, arg)
      case arg@TypedArgument.LongFlag(flag, _) ⇒ GeneralArgument.LongFlag(flag, arg)
      case arg@TypedArgument.PositionArg(_)    ⇒ GeneralArgument.PositionArg(arg)
    }

}