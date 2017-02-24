package com.github.mdr.mash.ns.maths

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions._
import com.github.mdr.mash.runtime._
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

import scala.collection.immutable.ListMap

object StatsFunction extends MashFunction("maths.stats") {

  object Params {
    val Sequence = Parameter(
      nameOpt = Some("sequence"),
      summaryOpt = Some("Sequence of numbers on which to compute statistics"))
  }
  import Params._

  val params = ParameterModel(Seq(Sequence))

  def apply(boundParams: BoundParams): MashObject = {
    val numbers: Seq[Double] =
      boundParams(Sequence) match {
        case list: MashList ⇒ list.elements.map {
          case MashNumber(n, _) ⇒ n
          case value            ⇒ boundParams.throwInvalidArgument(Sequence, "Invalid item of type " + value.typeName)
        }
        case value ⇒ boundParams.throwInvalidArgument(Sequence, "Invalid sequence of type " + value.typeName)
      }
    val stats = new DescriptiveStatistics
    numbers.foreach(stats.addValue)
    asMashObject(stats)
  }

  private def asMashObject(stats: DescriptiveStatistics): MashObject = {
    import StatsClass.Fields._
    MashObject.of(
      ListMap(
        Min -> MashNumber(stats.getMin),
        Max -> MashNumber(stats.getMax),
        Mean -> MashNumber(stats.getMean),
        StandardDeviation -> MashNumber(stats.getStandardDeviation),
        Median -> MashNumber(stats.getPercentile(50)),
        Count -> MashNumber(stats.getN)),
      StatsClass)
  }

  override def typeInferenceStrategy = StatsClass

  override def summaryOpt = Some("Compute standard statistics (mean, standard deviation etc) on a collection of numbers")
}