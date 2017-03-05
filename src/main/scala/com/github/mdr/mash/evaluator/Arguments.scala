package com.github.mdr.mash.evaluator

case class Arguments(evaluatedArguments: Seq[EvaluatedArgument[SuspendedMashValue]] = Seq()) {

  def positionArgs: Seq[EvaluatedArgument.PositionArg[SuspendedMashValue]] =
    evaluatedArguments.collect { case arg: EvaluatedArgument.PositionArg[SuspendedMashValue] ⇒ arg }

  lazy val argSet: Set[String] = evaluatedArguments.collect {
    case EvaluatedArgument.ShortFlag(flags, _)     ⇒ flags
    case EvaluatedArgument.LongFlag(flag, None, _) ⇒ Seq(flag)
  }.flatten.toSet

  def argValues: Map[String, SuspendedMashValue] = evaluatedArguments.collect {
    case EvaluatedArgument.LongFlag(flag, Some(value), _) ⇒ flag -> value
  }.toMap

  def isProvidedAsNamedArg(name: String): Boolean = argSet.contains(name) || argValues.contains(name)

}