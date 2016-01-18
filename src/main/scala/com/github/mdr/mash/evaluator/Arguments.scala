package com.github.mdr.mash.evaluator

case class Arguments(evaluatedArguments: Seq[EvaluatedArgument]) {

  def positionArgs: Seq[Any] = evaluatedArguments.collect { case EvaluatedArgument.PositionArg(value) ⇒ value }

  def argSet: Set[String] = evaluatedArguments.collect {
    case EvaluatedArgument.ShortFlag(flags)     ⇒ flags
    case EvaluatedArgument.LongFlag(flag, None) ⇒ Seq(flag)
  }.flatten.toSet

  def argValues: Map[String, Any] = evaluatedArguments.collect {
    case EvaluatedArgument.LongFlag(flag, Some(value)) ⇒ flag -> value
  }.toMap

  def isProvidedAsNamedArg(name: String): Boolean = argSet.contains(name) || argValues.contains(name)
  
}