package com.github.mdr.mash.ns.core

import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.ns.os.WithinFunction
import com.github.mdr.mash.inference.TypeInferenceStrategy
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.inference.Inferencer
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.completions.CompletionSpec
import java.time.Clock
import com.github.mdr.mash.runtime.MashObject
import scala.collection.immutable.ListMap
import com.github.mdr.mash.runtime.MashWrapped
import java.time.Duration
import com.github.mdr.mash.ns.os.ProcessResultClass

object TimeTakenFunction extends MashFunction("core.timeTaken") {

  private val clock: Clock = Clock.systemDefaultZone

  object Params {
    val Block = Parameter(
      name = "block",
      summary = "Code to execute",
      isLazy = true)
  }
  import Params._

  val params = ParameterModel(Seq(Block))

  def apply(arguments: Arguments): MashObject = {
    val boundParams = params.validate(arguments)
    val f = boundParams(Block).asInstanceOf[MashFunction]
    val start = clock.instant
    val result = f.apply(Arguments(Seq()))
    val finish = clock.instant
    val duration = ProcessResultClass.DurationMethod.durationBetween(start, finish)
    import TimedResultClass.Fields._
    MashObject.of(
      ListMap(
        Result -> result,
        Started -> MashWrapped(start),
        Duration_ -> duration),
      TimedResultClass)
  }

  override def typeInferenceStrategy = TimeTakenTypeInferenceStrategy

  override def summary = "Measure how long it takes to execute a piece of code"

  override def descriptionOpt = Some("""Examples:
   timeTaken (ls --recursive) | .duration # 854
""")

}

object TimeTakenTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    import WithinFunction.Params._
    val argBindings = TimeTakenFunction.params.bindTypes(arguments)
    val resultType = argBindings.get(Block).flatMap(_.typeOpt).getOrElse(Type.Any)
    Some(Type.TimedResult(resultType))
  }

}