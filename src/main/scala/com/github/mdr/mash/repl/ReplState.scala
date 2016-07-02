package com.github.mdr.mash.repl

import scala.collection.mutable

import com.github.mdr.mash.Config
import com.github.mdr.mash.ConfigOption
import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.evaluator.StandardEnvironment
import com.github.mdr.mash.incrementalSearch.IncrementalSearchState
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.utils.Region

case class YankLastArgState(count: Int, region: Region)

object ReplState {

  /**
   * Name of the 'it' variable, which stores the last result
   */
  val It = "it"

  /**
   * Name of the 'res' list, which stores the list of commands executed in the session.
   */
  val Res = "res"

}

class ReplState(
    var lineBuffer: LineBuffer = LineBuffer.Empty,
    var commandNumber: Int = 0,
    var completionStateOpt: Option[CompletionState] = None,
    var assistanceStateOpt: Option[AssistanceState] = None,
    var continue: Boolean = true, // Whether to loop or exit
    var globalVariables: MashObject = StandardEnvironment.createGlobalVariables(),
    var incrementalSearchStateOpt: Option[IncrementalSearchState] = None,
    var mish: Boolean = false,
    var yankLastArgStateOpt: Option[YankLastArgState] = None,
    var objectBrowserStateOpt: Option[ObjectBrowserState] = None) {

  import ReplState._

  def reset() {
    lineBuffer = LineBuffer.Empty
    completionStateOpt = None
    assistanceStateOpt = None
    incrementalSearchStateOpt = None
    yankLastArgStateOpt = None
  }

  def updateLineBuffer(transformation: LineBuffer ⇒ LineBuffer) {
    this.lineBuffer = transformation(this.lineBuffer)
  }

  def mode: ReplMode =
    if (objectBrowserStateOpt.isDefined)
      ReplMode.ObjectBrowser
    else
      completionStateOpt match {
        case Some(_: IncrementalCompletionState)         ⇒ ReplMode.IncrementalCompletions
        case Some(_: BrowserCompletionState)             ⇒ ReplMode.BrowseCompletions
        case None if incrementalSearchStateOpt.isDefined ⇒ ReplMode.IncrementalSearch
        case None                                        ⇒ ReplMode.Normal
      }

  private def getConfigObject: Option[MashObject] = globalVariables.get(StandardEnvironment.Config).flatMap(_.asObject)

  private def getBooleanConfig(configOption: ConfigOption): Boolean = {
    val rawValue = Config.getConfig(getConfigObject, configOption)
    rawValue.isTruthy
  }

  def bareWords: Boolean = getBooleanConfig(Config.Language.BareWords)

  def showStartupTips: Boolean = getBooleanConfig(Config.Cli.ShowStartupTips)

}
