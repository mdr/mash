package com.github.mdr.mash

import com.github.mdr.mash.assist.AssistanceState
import com.github.mdr.mash.incrementalSearch.IncrementalSearchState
import scala.collection.mutable
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.utils.Region
import scala.PartialFunction.cond
import com.github.mdr.mash.evaluator.MashObject

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
  
  val BareWordsConfigKey = "language.bareWords"
  val ShowStartupTipsConfigKey = "cli.showStartupTips"

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

  private def getConfig(configKey: String): Option[Any] =
    globalVariables("config") match {
      case obj: MashObject ⇒ getConfig(obj, configKey.split("\\."))
      case _               ⇒ None
    }

  private def getConfig(mo: MashObject, path: Seq[String]): Option[Any] = {
    for {
      first ← path.headOption
      rest = path.tail
      firstValue ← mo.getField(first)
      restValue ← rest match {
        case Seq() ⇒
          Some(firstValue)
        case _ ⇒
          firstValue match {
            case obj: MashObject ⇒ getConfig(obj, rest)
            case _               ⇒ None
          }
      }
    } yield restValue
  }

  def bareWords: Boolean = getConfig(BareWordsConfigKey).collect { case b: Boolean ⇒ b }.getOrElse(false)

  def showStartupTips: Boolean = getConfig(ShowStartupTipsConfigKey).collect { case b: Boolean ⇒ b }.getOrElse(false)

}
