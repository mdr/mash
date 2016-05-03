package com.github.mdr.mash.repl

import java.io.File
import java.io.PrintStream
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import org.apache.commons.io.FileUtils
import com.github.mdr.mash.CommandRunner
import com.github.mdr.mash.DebugLogger
import com.github.mdr.mash.MishCommand
import com.github.mdr.mash.Singletons
import com.github.mdr.mash.assist.InvocationAssistance
import com.github.mdr.mash.completions.CompletionResult
import com.github.mdr.mash.completions.UberCompleter
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.input.InputAction
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.screen.ReplRenderResult
import com.github.mdr.mash.screen.ReplRenderer
import com.github.mdr.mash.terminal.Terminal
import com.github.mdr.mash.tips.Tips
import java.nio.charset.StandardCharsets
import com.github.mdr.mash.os.FileSystem
import com.github.mdr.mash.os.EnvironmentInteractions
import java.nio.file.Paths
import java.nio.file.Files
import com.github.mdr.mash.input.NormalKeyMap
import com.github.mdr.mash.input.BrowseCompletionsKeyMap

object Repl {

  val MashRcPath = History.mashDir.resolve("mashrc")

}

class Repl(
  protected val terminal: Terminal,
  protected val output: PrintStream,
  fileSystem: FileSystem,
  envInteractions: EnvironmentInteractions)
    extends NormalActionHandler
    with IncrementalCompletionActionHandler
    with IncrementalSearchActionHandler
    with BrowseCompletionActionHandler {

  import Repl._

  protected val completer = new UberCompleter(fileSystem, envInteractions)

  val state = new ReplState()
  protected var previousReplRenderResultOpt: Option[ReplRenderResult] = None

  // TODO: obviously this is horrible, will be fixed when DI gets sorted out
  Singletons.history = state.history

  def run() {
    processMashRc()
    if (state.showStartupTips)
      Tips.showTip(output, terminal.info)
    inputLoop()
  }

  private def getMashRcLines: Seq[String] = {
    if (Files.exists(MashRcPath)) {
      try
        FileUtils.readLines(MashRcPath.toFile, StandardCharsets.UTF_8).asScala
      catch {
        case e: Exception ⇒
          output.println("Error reading " + MashRcPath)
          e.printStackTrace(output)
          DebugLogger.logException(e)
          Seq()
      }
    } else
      Seq()
  }

  private def processMashRc() {
    val lines = getMashRcLines
    val commandRunner = new CommandRunner(output, terminal.info, getEnvironment)
    for (line ← lines) {
      try
        commandRunner.run(line, state.mish, state.bareWords)
      catch {
        case e: Exception ⇒
          output.println("Error executing: " + line)
          e.printStackTrace(output)
          DebugLogger.logException(e)
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
    try {
      draw()
    } catch {
      case e: Exception ⇒
        DebugLogger.logException(e)
        state.lineBuffer = LineBuffer.Empty
        draw()
    }
    try {
      val action = fetchAction()
      handleAction(action)
    } catch {
      case e: Throwable ⇒
        e.printStackTrace(output)
        DebugLogger.logException(e)
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
      case ReplMode.BrowseCompletions ⇒ BrowseCompletionsKeyMap
      case _                          ⇒ NormalKeyMap
    }
    InputAction.fetchAction(isLineEmpty, keyMap)
  }

  def handleAction(action: InputAction) {
    state.completionStateOpt match {
      case Some(completionState: IncrementalCompletionState) ⇒ handleIncrementalCompletionAction(action, completionState)
      case Some(completionState: BrowserCompletionState)     ⇒ handleBrowserCompletionAction(action, completionState)
      case None ⇒
        state.incrementalSearchStateOpt match {
          case Some(searchState) ⇒
            handleIncrementalSearchAction(action, searchState)
          case None ⇒
            handleNormalAction(action)
        }
    }
    if (state.assistanceStateOpt.isDefined)
      updateInvocationAssistance()
  }

  protected def updateInvocationAssistance() {
    val text = state.lineBuffer.text
    val pos = state.lineBuffer.cursorPos
    state.assistanceStateOpt =
      text match {
        case MishCommand(prefix, mishCmd) ⇒
          val newPos = pos - prefix.length // adjust for the prefix
          if (newPos >= 0)
            InvocationAssistance.getCallingSyntaxOfCurrentInvocation(text, newPos, getEnvironment, mish = true)
          else
            None
        case _ ⇒
          InvocationAssistance.getCallingSyntaxOfCurrentInvocation(text, pos, getEnvironment, mish = state.mish)
      }
  }

  /**
   * Attempt completions at the current position (doesn't change the state).
   */
  protected def complete: Option[CompletionResult] = {
    val text = state.lineBuffer.text
    val pos = state.lineBuffer.cursorPos

    text match {
      case MishCommand(prefix, mishCmd) ⇒
        val shift = prefix.length // adjust for the prefix
        val newPos = pos - shift
        if (newPos >= 0)
          completer.complete(mishCmd, pos = newPos, getEnvironment, mish = true).map(_.translate(shift))
        else
          None
      case _ ⇒
        completer.complete(text, pos, getEnvironment, mish = state.mish)
    }
  }

  protected def getEnvironment = Environment(Map(), state.globalVariables)

}
