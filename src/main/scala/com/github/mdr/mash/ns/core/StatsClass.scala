package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.Field

object StatsClass extends MashClass("maths.Stats") {

  object Fields {
    val Min = Field("min", "Smallest value in data set", NumberClass)
    val Max = Field("max", "Largest value in data set", NumberClass)
    val Mean = Field("mean", "Average value in the data set", NumberClass)
    val StandardDeviation = Field("standardDeviation", "Standard deviation", NumberClass)
    val Median = Field("median", "Median value of the data set", NumberClass)
    val Count = Field("count", "Number of elements in the data set", NumberClass)
  }

  import Fields._

  override val fields = Seq(Min, Max, Mean, StandardDeviation, Median, Count)

  override def summary = "Statistics for a data set"

}