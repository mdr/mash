package com.github.mdr.mash.ns.collections

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.functions._
import com.github.mdr.mash.ns.collections.listClass._
import com.github.mdr.mash.ns.core._

object ListClass extends MashClass("collections.List") {

  override val methods = Seq(
    methodise(AllFunction),
    methodise(AllButLastFunction),
    methodise(AnyFunction),
    methodise(ChunkedFunction),
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
    CartesianProductMethod,
    IntersectMethod,
    SelectMethod)

  override val staticMethods = Seq(NewStaticMethod)

  def methodise(function: MashFunction, methodAliases: Seq[String] = Seq()): MashMethod =
    FunctionWrappingMethod(function, methodAliases)

  override def summaryOpt = Some("A sequence of objects")

  override def parentOpt = Some(AnyClass)

}

