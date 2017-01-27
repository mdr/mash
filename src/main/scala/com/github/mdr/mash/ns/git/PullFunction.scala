package com.github.mdr.mash.ns.git

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, ParameterModel }
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.runtime.MashList

import scala.collection.JavaConverters._

object PullFunction extends MashFunction("git.pull") {

  val params = ParameterModel(Seq())

  def apply(arguments: Arguments): MashList = {
    params.validate(arguments)
    GitHelper.withGit { git ⇒
      val pullResult = git.pull.call()
      val fetchResult = pullResult.getFetchResult
      val updates = fetchResult.getTrackingRefUpdates.asScala.toSeq
      MashList(updates.map(FetchFunction.asMashObject))
    }
  }

  override def typeInferenceStrategy = Seq(FetchBranchUpdateClass)

  override def summaryOpt = Some("Fetch from and integrate with another repository or a local branch")

}