package com.github.mdr.mash.ns.core

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.collections.{ AnyFunction, _ }
import com.github.mdr.mash.ns.core.stringClass._

object StringClass extends MashClass("core.String") {

  override val methods = Seq(
    ListClass.methodise(AllFunction),
    ListClass.methodise(AllButLastFunction),
    ListClass.methodise(AnyFunction),
    ListClass.methodise(ChunkedFunction),
    ListClass.methodise(CountMatchesFunction),
    ListClass.methodise(EachFunction),
    ListClass.methodise(FlatMapFunction),
    ListClass.methodise(FindFunction),
    ListClass.methodise(FirstFunction),
    ListClass.methodise(GroupByFunction),
    ListClass.methodise(IndexOfFunction),
    ListClass.methodise(IsEmptyFunction),
    ListClass.methodise(JoinFunction),
    ListClass.methodise(LastFunction),
    ListClass.methodise(LengthFunction, Seq("count")),
    ListClass.methodise(MapFunction),
    ListClass.methodise(MaxByFunction),
    ListClass.methodise(MaxFunction),
    ListClass.methodise(MinByFunction),
    ListClass.methodise(MinFunction),
    ListClass.methodise(NonEmptyFunction),
    ListClass.methodise(ReduceFunction),
    ListClass.methodise(ReverseFunction),
    ListClass.methodise(SkipFunction),
    ListClass.methodise(SkipUntilFunction),
    ListClass.methodise(SkipWhileFunction),
    ListClass.methodise(SlidingFunction),
    ListClass.methodise(SortByFunction),
    ListClass.methodise(SortFunction),
    ListClass.methodise(SumByFunction),
    ListClass.methodise(SumFunction),
    ListClass.methodise(TakeWhileFunction),
    ListClass.methodise(UniqueFunction),
    ListClass.methodise(WhereFunction),
    ListClass.methodise(WhereNotFunction),
    CharMethod,
    ContainsMethod,
    EndsWithMethod,
    GlobMethod,
    GrepMethod,
    LinesMethod,
    MatchesMethod,
    RegexMethod,
    ReplaceMethod,
    StartsWithMethod,
    SplitMethod,
    TagMethod,
    ToDateMethod,
    ToDateTimeMethod,
    ToLowerMethod,
    ToNumberMethod,
    ToPathMethod,
    ToUpperMethod,
    TrimMethod,
    UntaggedMethod)

  override def summaryOpt = Some("A string")

  def taggedWith(klass: MashClass) = Type.Tagged(this, klass)

  override def parentOpt = Some(AnyClass)

}

object SameStringMethodTypeInferenceStrategy extends MethodTypeInferenceStrategy {

  override def inferTypes(inferencer: Inferencer, targetTypeOpt: Option[Type], arguments: TypedArguments): Option[Type] =
    targetTypeOpt.orElse(Some(Type.Instance(StringClass)))

}