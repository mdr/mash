package com.github.mdr.mash.evaluator

case class Arguments(evaluatedArguments: Seq[EvaluatedArgument]) {

  def positionArgs: Seq[EvaluatedArgument.PositionArg] =
    evaluatedArguments.collect { case arg: EvaluatedArgument.PositionArg ⇒ arg }

  lazy val argSet: Set[String] = evaluatedArguments.collect {
    case EvaluatedArgument.ShortFlag(flags, _)     ⇒ flags
    case EvaluatedArgument.LongFlag(flag, None, _) ⇒ Seq(flag)
  }.flatten.toSet

  def argValues: Map[String, Any] = evaluatedArguments.collect {
    case EvaluatedArgument.LongFlag(flag, Some(value), _) ⇒ flag -> value
  }.toMap

  def isProvidedAsNamedArg(name: String): Boolean = argSet.contains(name) || argValues.contains(name)

}