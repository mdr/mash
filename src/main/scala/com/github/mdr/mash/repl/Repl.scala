package com.github.mdr.mash.repl

import java.io.PrintStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import org.apache.commons.io.FileUtils
import com.github.mdr.mash.commands.CommandRunner
import com.github.mdr.mash.DebugLogger
import com.github.mdr.mash.Mash
import com.github.mdr.mash.commands.MishCommand
import com.github.mdr.mash.assist.InvocationAssistance
import com.github.mdr.mash.completions.Completer
import com.github.mdr.mash.completions.CompletionResult
import com.github.mdr.mash.input.BrowseCompletionsKeyMap
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.input.NormalKeyMap
import com.github.mdr.mash.input.ObjectBrowserKeyMap
import com.github.mdr.mash.os.EnvironmentInteractions
import com.github.mdr.mash.os.FileSystem
import com.github.mdr.mash.repl.history.History
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.screen.ReplRenderResult
import com.github.mdr.mash.screen.ReplRenderer
import com.github.mdr.mash.terminal.Terminal
import com.github.mdr.mash.tips.Tips
import com.github.mdr.mash.compiler.CompilationUnit
import java.util.UUID

object Repl {

  val InitFile = "init.mash"
  val InitPath = Mash.MashDir.resolve(InitFile)

}

class Repl(
  protected val terminal: Terminal,
  protected val output: PrintStream,
  fileSystem: FileSystem,
  envInteractions: EnvironmentInteractions,
  protected val history: History,
  protected val sessionId: UUID)
    extends NormalActionHandler
    with IncrementalCompletionActionHandler
    with IncrementalSearchActionHandler
    with BrowseCompletionActionHandler
    with ObjectBrowserActionHandler {

  import Repl._

  protected val debugLogger = new DebugLogger(sessionId)
  protected val completer = new Completer(fileSystem, envInteractions)

  val state = new ReplState
  protected var previousReplRenderResultOpt: Option[ReplRenderResult] = None

  def run() {
    processInitFile()
    if (state.showStartupTips)
      Tips.showTip(output, terminal.info)
    inputLoop()
  }

  private def getInitScript: Option[CompilationUnit] = {
    Mash.ensureMashDirExists()
    if (Files.exists(InitPath))
      try {
        val s = FileUtils.readFileToString(InitPath.toFile, StandardCharsets.UTF_8)
        Some(CompilationUnit(s, name = InitFile, mish = false))
      } catch {
        case e: Exception ⇒
          output.println("Error reading " + InitPath)
          e.printStackTrace(output)
          debugLogger.logException(e)
          None
      }
    else
      None
  }

  private def processInitFile() {
    for (initScript ← getInitScript) {
      val commandRunner = new CommandRunner(output, terminal.info, state.globalVariables, sessionId)
      try
        commandRunner.runCompilationUnit(initScript, state.bareWords)
      catch {
        case e: Exception ⇒
          output.println(s"Error executing $InitFile")
          e.printStackTrace(output)
          debugLogger.logException(e)
          return
      }
    }
  }

  def draw() {
    val screenRenderResult = ReplRenderer.render(state, terminal.info)
    val previousScreenOpt = previousReplRenderResultOpt.map(_.screen)
    val drawn = screenRenderResult.screen.draw(previousScreenOpt, terminal.columns)
    previousReplRenderResultOpt = Some(screenRenderResult)

    output.write(drawn.getBytes)
    output.flush()
  }

  @tailrec
  private def inputLoop() {
    try
      draw()
    catch {
      case e: Exception ⇒
        debugLogger.logException(e)
        state.reset()
        draw()
    }
    try {
      val action = fetchAction()
      handleAction(action)
    } catch {
      case e: Throwable ⇒
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
    val isLineEmpty = state.lineBuffer.isEmpty
    val keyMap = state.mode match {
      case ReplMode.ObjectBrowser     ⇒ ObjectBrowserKeyMap
      case ReplMode.BrowseCompletions ⇒ BrowseCompletionsKeyMap
      case _                          ⇒ NormalKeyMap
    }
    InputAction.fetchAction(isLineEmpty, keyMap)
  }

  def handleAction(action: InputAction) {
    state.objectBrowserStateOpt match {
      case Some(state) ⇒
        handleObjectBrowserAction(action, state)
      case None ⇒
        state.completionStateOpt match {
          case Some(completionState: IncrementalCompletionState) ⇒ handleIncrementalCompletionAction(action, completionState)
          case Some(completionState: BrowserCompletionState)     ⇒ handleBrowserCompletionAction(action, completionState)
          case None ⇒
            state.incrementalSearchStateOpt match {
              case Some(searchState) ⇒ handleIncrementalSearchAction(action, searchState)
              case None              ⇒ handleNormalAction(action)
            }
        }
        if (state.assistanceStateOpt.isDefined)
          updateInvocationAssistance()
    }
  }

  protected def updateInvocationAssistance() {
    val text = state.lineBuffer.text
    val pos = state.lineBuffer.cursorOffset
    val newAssistanceStateOpt =
      text match {
        case MishCommand(prefix, mishCmd) ⇒
          val newPos = pos - prefix.length // adjust for the prefix
          if (newPos >= 0)
            InvocationAssistance.getCallingSyntaxOfCurrentInvocation(text, newPos, getBindings, mish = true)
          else
            None
        case _ ⇒
          InvocationAssistance.getCallingSyntaxOfCurrentInvocation(text, pos, getBindings, mish = state.mish)
      }
    state.assistanceStateOpt = newAssistanceStateOpt orElse state.assistanceStateOpt
  }

  /**
   * Attempt completions at the current position (doesn't change the state).
   */
  protected def complete: Option[CompletionResult] = {
    val text = state.lineBuffer.text
    val pos = state.lineBuffer.cursorOffset

    text match {
      case MishCommand(prefix, mishCmd) ⇒
        val shift = prefix.length // adjust for the prefix
        val newPos = pos - shift
        if (newPos >= 0)
          completer.complete(mishCmd, pos = newPos, getBindings, mish = true).map(_.translate(shift))
        else
          None
      case _ ⇒
        completer.complete(text, pos, getBindings, mish = state.mish)
    }
  }

  private def getBindings: Map[String, MashValue] = state.globalVariables.immutableFields

}
