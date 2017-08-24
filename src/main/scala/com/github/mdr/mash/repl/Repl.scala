package com.github.mdr.mash.repl

import java.io.PrintStream
import java.util.UUID

import com.github.mdr.mash.assist.InvocationAssistanceUpdater
import com.github.mdr.mash.commands.MishCommand
import com.github.mdr.mash.completions.{ Completer, CompletionResult }
import com.github.mdr.mash.input._
import com.github.mdr.mash.os.{ EnvironmentInteractions, FileSystem }
import com.github.mdr.mash.printer.ViewConfig
import com.github.mdr.mash.render.ReplRenderer
import com.github.mdr.mash.repl.browser.handler.ObjectBrowserActionHandler
import com.github.mdr.mash.repl.completions.{ BrowseCompletionActionHandler, BrowserCompletionState, IncrementalCompletionActionHandler, IncrementalCompletionState }
import com.github.mdr.mash.repl.handler.{ IncrementalHistorySearchActionHandler, NormalActionHandler }
import IncrementalHistorySearchActionHandler.Result
import com.github.mdr.mash.repl.history.History
import com.github.mdr.mash.runtime.{ MashObject, MashValue }
import com.github.mdr.mash.screen.{ Screen, ScreenDrawer }
import com.github.mdr.mash.terminal.Terminal
import com.github.mdr.mash.terminal.ansi.EscapeSequence
import com.github.mdr.mash.tips.Tips
import com.github.mdr.mash.{ ConfigWrapper, DebugLogger }

import scala.annotation.tailrec
import scala.util.control.NonFatal

class Repl(protected val terminal: Terminal,
           protected val output: PrintStream,
           fileSystem: FileSystem,
           envInteractions: EnvironmentInteractions,
           protected val history: History,
           protected val sessionId: UUID,
           val globalVariables: MashObject)
  extends NormalActionHandler
    with IncrementalCompletionActionHandler
    with BrowseCompletionActionHandler
    with ObjectBrowserActionHandler {

  protected val debugLogger = new DebugLogger(sessionId.toString)
  protected val completer = new Completer(fileSystem, envInteractions)

  var state = new ReplState()

  protected var previousScreenOpt: Option[Screen] = None

  private def config: ConfigWrapper = ConfigWrapper.fromGlobals(globalVariables)

  def bareWords: Boolean = config.bareWords

  def showStartupTips: Boolean = config.showStartupTips

  def viewConfig: ViewConfig = ViewConfig(config.viewFuzzyTime, config.browseLargeOutput)

  def run() {
    if (showStartupTips)
      Tips.showTip(output, terminal.size)
    inputLoop()
  }

  def draw() {
    val newScreen = render
    val drawn = ScreenDrawer.draw(newScreen, previousScreenOpt)
    previousScreenOpt = Some(newScreen)

    output.write(drawn.getBytes)
    output.flush()
  }

  def render: Screen = new ReplRenderer(fileSystem, envInteractions, terminal.size, globalVariables, bareWords).render(state)

  @tailrec
  private def inputLoop() {
    try
      draw()
    catch {
      case NonFatal(e) ⇒
        debugLogger.logException(e)
        state = state.reset
        draw()
    }
    try {
      val action = fetchAction()
      handleAction(action)
    } catch {
      case NonFatal(e) ⇒
        e.printStackTrace(output)
        debugLogger.logException(e)
    }
    if (state.continue)
      inputLoop()
    else {
      draw() // Tidy up before exiting
      output.println()
    }
  }

  private def fetchAction(): InputAction = {
    val replMode = ReplMode.getMode(state)
    val isLineEmpty = replMode == ReplMode.Normal && state.lineBuffer.isEmpty
    val keyMap = replMode match {
      case ReplMode.ObjectBrowser                   ⇒ ObjectBrowserKeyMap
      case ReplMode.ObjectBrowser.IncrementalSearch ⇒ ObjectBrowserKeyMap.IncrementalSearch
      case ReplMode.ObjectBrowser.ExpressionInput   ⇒ ObjectBrowserKeyMap.ExpressionInput
      case ReplMode.BrowseCompletions               ⇒ BrowseCompletionsKeyMap
      case ReplMode.IncrementalSearch               ⇒ IncrementalHistorySearchKeyMap
      case _                                        ⇒ NormalKeyMap
    }
    InputAction.fetchAction(isLineEmpty, keyMap)
  }

  def handleAction(action: InputAction) {
    state.objectBrowserStateStackOpt match {
      case Some(stateStack) ⇒
        handleObjectBrowserAction(action, stateStack)
      case None             ⇒
        state.completionStateOpt match {
          case Some(completionState: IncrementalCompletionState) ⇒ handleIncrementalCompletionAction(action, completionState)
          case Some(completionState: BrowserCompletionState)     ⇒ handleBrowserCompletionAction(action, completionState)
          case None                                              ⇒
            state.incrementalHistorySearchStateOpt match {
              case Some(searchState) ⇒ handleHistorySearchAction(action)
              case None              ⇒ handleNormalAction(action)
            }
        }
        if (state.assistanceStateOpt.isDefined)
          state = InvocationAssistanceUpdater.updateInvocationAssistance(state, getBindings)
    }
  }

  private def handleHistorySearchAction(action: InputAction) = {
    val Result(newState, actionConsumed) = IncrementalHistorySearchActionHandler(history, fileSystem).handleAction(action, state)
    state = newState
    if (!actionConsumed)
      handleNormalAction(action)
  }

  /**
    * Attempt completions at the current position (doesn't change the state).
    */
  protected def complete: Option[CompletionResult] = complete(state.lineBuffer, state.mish)

  protected def complete(lineBuffer: LineBuffer, mish: Boolean): Option[CompletionResult] = {
    val text = lineBuffer.text
    val pos = lineBuffer.cursorOffset

    text match {
      case MishCommand(prefix, mishCmd) ⇒
        val shift = prefix.length // adjust for the prefix
      val newPos = pos - shift
        if (newPos >= 0)
          completer.complete(mishCmd, pos = newPos, getBindings, mish = true).map(_.translate(shift))
        else
          None
      case _                            ⇒
        completer.complete(text, pos, getBindings, mish = mish)
    }
  }

  protected def getBindings: Map[String, MashValue] = globalVariables.stringFields

  protected def clearScreen() {
    output.write(EscapeSequence.ClearScreen.getBytes)
    output.flush()
  }

}
