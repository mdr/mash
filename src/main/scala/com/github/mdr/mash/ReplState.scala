package com.github.mdr.mash

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.incrementalSearch.IncrementalSearchState
import scala.collection.mutable
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.utils.Region
import scala.PartialFunction.cond
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.Truthiness

sealed trait ReplMode
object ReplMode {
  case object Normal extends ReplMode
  case object BrowseCompletions extends ReplMode
  case object IncrementalCompletions extends ReplMode
  case object IncrementalSearch extends ReplMode
}

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

  def mode: ReplMode =
    completionStateOpt match {
      case Some(completionState: IncrementalCompletionState) ⇒ ReplMode.IncrementalCompletions
      case Some(completionState: BrowserCompletionState)     ⇒ ReplMode.BrowseCompletions
      case None ⇒
        incrementalSearchStateOpt match {
          case Some(searchState) ⇒
            ReplMode.IncrementalSearch
          case None ⇒
            ReplMode.Normal
        }

    }

  private def getConfig(configOption: ConfigOption): Any = {
    val configObjectOpt = globalVariables.get("config") match {
      case Some(mo: MashObject) ⇒ Some(mo)
      case _                    ⇒ None
    }
    Config.getConfig(configObjectOpt, configOption)
  }

  def bareWords: Boolean = Truthiness.isTruthy(getConfig(Config.Language.BareWords))
  def showStartupTips: Boolean = Truthiness.isTruthy(getConfig(Config.Cli.ShowStartupTips))

}
