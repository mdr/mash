package com.github.mdr.mash.ns.core

import java.time.Clock

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ Inferencer, Type, TypeInferenceStrategy, TypedArguments }
import com.github.mdr.mash.ns.os.{ ProcessResultClass, WithinFunction }
import com.github.mdr.mash.runtime.{ MashObject, MashWrapped }

import scala.collection.immutable.ListMap

object TimeTakenFunction extends MashFunction("core.timeTaken") {

  private val clock: Clock = Clock.systemDefaultZone

  object Params {
    val Block = Parameter(
      nameOpt = Some("block"),
      summaryOpt = Some("Code to execute"),
      isLazy = true)
  }
  import Params._

  val params = ParameterModel(Seq(Block))

  def apply(boundParams: BoundParams): MashObject = {
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

  override def summaryOpt = Some("Measure how long it takes to execute a piece of code")

  override def descriptionOpt = Some("""Examples:
   timeTaken (ls --recursive) | .duration # 854
""")

}

object TimeTakenTypeInferenceStrategy extends TypeInferenceStrategy {

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] = {
    import WithinFunction.Params._
    val argBindings = TimeTakenFunction.params.bindTypes(arguments)
    val resultType = argBindings.getType(Block).getOrElse(Type.Any)
    Some(Type.Generic(TimedResultClass, resultType))
  }

}