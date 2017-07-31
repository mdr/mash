package com.github.mdr.mash.ns.core.thread

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, ParameterModel }
import com.github.mdr.mash.inference.{ Type, TypedArguments }
import com.github.mdr.mash.ns.collections.{ MapFunction, MapTypeInferenceStrategy }
import com.github.mdr.mash.runtime.{ MashList, MashString, MashValue }

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }

object ParallelMapFunction extends MashFunction("core.thread.parallelMap") {

  import MapFunction.Params._

  val params = ParameterModel(F, Sequence)

  def call(boundParams: BoundParams): MashValue = {
    val inSequence = boundParams(Sequence)
    val sequence = boundParams.validateSequence(Sequence)
    val f = boundParams.validateFunction(F)
    val mapped = parallelMap(sequence, f)
    inSequence match {
      case MashString(_, tagOpt) if mapped.forall(_.isAString) ⇒
        mapped.asInstanceOf[Seq[MashString]].fold(MashString("", tagOpt))(_ + _)
      case _                                                                  ⇒
        MashList(mapped)
    }
  }

  private def parallelMap(sequence: Seq[MashValue], f: MashValue ⇒ MashValue): Seq[MashValue] = {
    val xs = Future.traverse(sequence)(x ⇒ Future(f(x)))
    Await.result(xs, Duration.Inf)
  }

  override def typeInferenceStrategy = MapTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = {
    val argBindings = MapFunction.params.bindTypes(arguments)
    val specOpt =
      for {
        param ← argBindings.paramAt(argPos)
        if param == F
        Type.Seq(elementType) ← argBindings.getType(Sequence)
      } yield CompletionSpec.Members(elementType)
    specOpt.toSeq
  }

  override def summaryOpt = Some("Transform each element of a sequence by a given function")

  override def descriptionOpt = Some(
    """The given function is applied to each element of the input sequence
  to produce a sequence of transformed output elements.

Examples:
<mash>
  parallelMap (_ * 2) [1, 2, 3] # [2, 4, 6]
</mash>""")
}
