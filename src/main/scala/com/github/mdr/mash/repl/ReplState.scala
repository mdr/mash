package com.github.mdr.mash.repl

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.incrementalSearch.IncrementalSearchState
import scala.collection.mutable
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.Truthiness
import com.github.mdr.mash.Config
import com.github.mdr.mash.ConfigOption

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
    var completionStateOpt: Option[CompletionState] = None,
    var assistanceStateOpt: Option[AssistanceState] = None,
    val history: History = new History(),
    var continue: Boolean = true, // Whether to loop or exit
    var globalVariables: mutable.Map[String, Any] = Environment.createGlobalVariables(),
    var incrementalSearchStateOpt: Option[IncrementalSearchState] = None,
    var mish: Boolean = false,
    var yankLastArgStateOpt: Option[YankLastArgState] = None) {

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
    completionStateOpt match {
      case Some(_: IncrementalCompletionState)         ⇒ ReplMode.IncrementalCompletions
      case Some(_: BrowserCompletionState)             ⇒ ReplMode.BrowseCompletions
      case None if incrementalSearchStateOpt.isDefined ⇒ ReplMode.IncrementalSearch
      case None                                        ⇒ ReplMode.Normal
    }

  private def getConfigObject: Option[MashObject] = globalVariables.get("config") collect {
    case mo: MashObject ⇒ mo
  }

  private def getBooleanConfig(configOption: ConfigOption): Boolean = {
    val rawValue = Config.getConfig(getConfigObject, configOption)
    Truthiness.isTruthy(rawValue)
  }

  def bareWords: Boolean = getBooleanConfig(Config.Language.BareWords)

  def showStartupTips: Boolean = getBooleanConfig(Config.Cli.ShowStartupTips)

}
