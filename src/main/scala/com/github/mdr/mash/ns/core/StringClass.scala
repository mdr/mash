package com.github.mdr.mash.ns.core

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.inference._
import com.github.mdr.mash.ns.collections.ListClass.methodise
import com.github.mdr.mash.ns.collections.{ AnyFunction, _ }
import com.github.mdr.mash.ns.core.stringClass._

object StringClass extends MashClass("core.String") {

  override val methods = Seq(
    methodise(AllFunction),
    methodise(AllButLastFunction),
    methodise(AnyFunction),
    methodise(ChunkedFunction),
    methodise(CountMatchesFunction),
    methodise(EachFunction),
    methodise(FlatMapFunction),
    methodise(FindFunction),
    methodise(FirstFunction),
    methodise(GroupByFunction),
    methodise(IsEmptyFunction),
    methodise(JoinFunction),
    methodise(LastFunction),
    methodise(LengthFunction, Seq("count")),
    methodise(MapFunction),
    methodise(MaxByFunction),
    methodise(MaxFunction),
    methodise(MinByFunction),
    methodise(MinFunction),
    methodise(NonEmptyFunction),
    methodise(ReduceFunction),
    methodise(ReverseFunction),
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
    ToCharacterMethod,
    ContainsMethod,
    EndsWithMethod,
    GlobMethod,
    GrepMethod,
    IndexOfMethod,
    LinesMethod,
    MatchesMethod,
    RegexMethod,
    ReplaceMethod,
    StartsWithMethod,
    SplitMethod,
    TagMethod,
    ToDateMethod,
    ToDateTimeMethod,
    ToListMethod,
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