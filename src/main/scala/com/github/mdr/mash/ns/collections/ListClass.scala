package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions._
import com.github.mdr.mash.inference.{ TypedArguments, _ }
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.runtime.{ MashList, MashNull, MashValue }

import scala.PartialFunction._

object ListClass extends MashClass("collections.List") {

  import MashClass.alias

  override val methods = Seq(
    methodise(AllFunction),
    methodise(AnyFunction),
    methodise(ContainsFunction),
    methodise(CountMatchesFunction),
    methodise(EachFunction),
    methodise(FlattenFunction),
    methodise(FlatMapFunction),
    methodise(FindFunction),
    methodise(FirstFunction),
    methodise(GrepFunction),
    methodise(GroupByFunction),
    methodise(IndexOfFunction),
    methodise(IsEmptyFunction),
    methodise(JoinFunction),
    methodise(LastFunction),
    methodise(LengthFunction),
    methodise(MapFunction),
    methodise(MaxByFunction),
    methodise(MaxFunction),
    methodise(MinByFunction),
    methodise(MinFunction),
    methodise(NonEmptyFunction),
    methodise(ReduceFunction),
    methodise(ReverseFunction),
    methodise(SelectFunction),
    methodise(SkipFunction),
    methodise(SkipUntilFunction),
    methodise(SkipWhileFunction),
    methodise(SlidingFunction),
    methodise(SortByFunction),
    methodise(SortFunction),
    methodise(SumByFunction),
    methodise(SumFunction),
    methodise(TakeWhileFunction),
    methodise(UniqueFunction),
    methodise(WhereFunction),
    methodise(WhereNotFunction),
    alias("count", methodise(LengthFunction)),
    alias("drop", methodise(SkipFunction)),
    alias("dropIf", methodise(WhereNotFunction)),
    alias("dropWhile", methodise(SkipWhileFunction)),
    alias("filter", methodise(WhereFunction)),
    alias("filterNot", methodise(WhereNotFunction)),
    alias("keepIf", methodise(WhereFunction)),
    IntersectMethod)

  object IntersectMethod extends MashMethod("intersect") {

    object Params {
      val Sequence = Parameter(
        nameOpt = Some("sequence"),
        summaryOpt = Some("Other sequence to intersect with this"))
    }
    import Params._

    override val params = ParameterModel(Seq(Sequence))

    def apply(target: MashValue, arguments: Arguments): MashList = {
      val boundParams = params.validate(arguments)
      val sequence = MashList(boundParams.validateSequence(Sequence))
      target.asInstanceOf[MashList] intersect sequence
    }

    override def summaryOpt: Option[String] = Some("Compute the multiset intersection between this and another sequence")

    object IntersectMethodTypeInferenceStrategy extends MethodTypeInferenceStrategy {

      def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] = {
        val argBindings = params.bindTypes(arguments)
        targetTypeOpt orElse argBindings.getArgument(Sequence).flatMap(_.typeOpt)
      }
    }

    override def typeInferenceStrategy = IntersectMethodTypeInferenceStrategy
  }

  def methodise(function: MashFunction): MashMethod = new MashMethod(function.name) {

    val params = function.params.copy(function.params.params.filterNot(_.nameOpt contains "sequence"))

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      val targetArg = EvaluatedArgument.PositionArg(SuspendedMashValue(() ⇒ target), None)
      function.apply(Arguments(arguments.evaluatedArguments :+ targetArg))
    }

    override def typeInferenceStrategy = (inferencer, targetTypeOpt, arguments) =>
      function.typeInferenceStrategy.inferTypes(inferencer, updateArgs(arguments, targetTypeOpt))

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments): Seq[CompletionSpec] =
      function.getCompletionSpecs(argPos, updateArgs(arguments, targetTypeOpt))

    private def updateArgs(arguments: TypedArguments, targetTypeOpt: Option[Type]): TypedArguments = {
      val sequenceArg = TypedArgument.PositionArg(ValueInfo(None, targetTypeOpt))
      TypedArguments(arguments.arguments :+ sequenceArg)
    }

    override def toString = s"methodise($function)"

    override def summaryOpt = function.summaryOpt

    override def descriptionOpt = function.descriptionOpt

  }

  override def summaryOpt = Some("A sequence of objects")

  override def parentOpt = Some(AnyClass)

}

