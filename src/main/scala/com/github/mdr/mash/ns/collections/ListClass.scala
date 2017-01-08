package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ Flag, MashFunction, MashMethod }
import com.github.mdr.mash.inference.{ TypedArguments, _ }
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.runtime.MashValue

object ListClass extends MashClass("collections.List") {

  import MashClass.alias

  override val methods = Seq(
    methodise(AllFunction),
    methodise(AnyFunction),
    methodise(ContainsFunction),
    methodise(CountMatchesFunction),
    methodise(EachFunction),
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
    alias("discardIf", methodise(WhereNotFunction)),
    alias("drop", methodise(SkipFunction)),
    alias("dropIf", methodise(WhereNotFunction)),
    alias("dropWhile", methodise(SkipWhileFunction)),
    alias("filter", methodise(WhereFunction)),
    alias("filterNot", methodise(WhereNotFunction)),
    alias("keepIf", methodise(WhereFunction)))

  private def methodise(function: MashFunction): MashMethod = new MashMethod(function.nameOpt.get) {

    val params = function.params.copy(function.params.params.filterNot(_.name == "sequence"))

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      val targetArg = EvaluatedArgument.PositionArg(SuspendedMashValue(() ⇒ target), None)
      function.apply(Arguments(arguments.evaluatedArguments :+ targetArg))
    }

    override def typeInferenceStrategy = new MethodTypeInferenceStrategy() {
      def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] =
        function.typeInferenceStrategy.inferTypes(inferencer, updateArgs(arguments, targetTypeOpt))
    }

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments): Seq[CompletionSpec] =
      function.getCompletionSpecs(argPos, updateArgs(arguments, targetTypeOpt))

    private def updateArgs(arguments: TypedArguments, targetTypeOpt: Option[Type]): TypedArguments = {
      val sequenceArg = TypedArgument.PositionArg(ValueInfo(None, targetTypeOpt))
      TypedArguments(arguments.arguments :+ sequenceArg)
    }

    override def flags: Seq[Flag] = params.flags

    override def toString = s"methodise($function)"

    override def summary = function.summary

    override def descriptionOpt = function.descriptionOpt

  }

  override def summary = "A sequence of objects"

  override def parentOpt = Some(AnyClass)

}

