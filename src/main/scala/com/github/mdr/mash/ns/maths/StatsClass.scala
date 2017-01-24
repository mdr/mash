package com.github.mdr.mash.ns.maths

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.ns.core.NumberClass

object StatsClass extends MashClass("maths.Stats") {

  object Fields {
    val Min = Field("min", Some("Smallest value in data set"), NumberClass)
    val Max = Field("max", Some("Largest value in data set"), NumberClass)
    val Mean = Field("mean", Some("Average value in the data set"), NumberClass)
    val StandardDeviation = Field("standardDeviation", Some("Standard deviation"), NumberClass)
    val Median = Field("median", Some("Median value of the data set"), NumberClass)
    val Count = Field("count", Some("Number of elements in the data set"), NumberClass)
  }

  import Fields._

  override val fields = Seq(Min, Max, Mean, StandardDeviation, Median, Count)

  override def summaryOpt = Some("Statistics for a data set")

}