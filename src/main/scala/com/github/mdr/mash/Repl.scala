package com.github.mdr.mash

import scala.annotation.tailrec
import com.github.mdr.mash.assist.InvocationAssistance
import com.github.mdr.mash.completions.UberCompleter
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.printer.TerminalInfo
import com.github.mdr.mash.screen.ReplRenderResult
import com.github.mdr.mash.screen.ReplRenderer
import jline.Terminal
import com.github.mdr.mash.completions.CompletionResult
import org.apache.commons.io.IOUtils
import java.io.File
import org.apache.commons.io.FileUtils
import scala.collection.JavaConverters._

class Repl(terminal: Terminal)
    extends NormalActionHandler
    with IncrementalCompletionActionHandler
    with IncrementalSearchActionHandler
    with BrowseCompletionActionHandler {

  private val fileSystem = LinuxFileSystem
  private val envInteractions = LinuxEnvironmentInteractions
  protected val completer = new UberCompleter(fileSystem, envInteractions)

  protected val state = new ReplState()
  protected var previousReplRenderResultOpt: Option[ReplRenderResult] = None

  def run() {
    initialise()
    inputLoop()
  }

  private def initialise() {
    val mashRc = new File(History.mashDir, "mashrc")
    if (mashRc.exists) {
      val lines =
        try
          FileUtils.readLines(mashRc).asScala
        catch {
          case e: Exception ⇒
            println("Error reading mashrc")
            e.printStackTrace()
            DebugLogger.logException(e)
            return
        }
      val commandRunner = new CommandRunner(terminalInfo, getEnvironment)
      for (line ← lines) {
        try
          commandRunner.run(line, state.mish, state.bareWords)
        catch {
          case e: Exception ⇒
            println("Error executing: " + line)
            e.printStackTrace()
            DebugLogger.logException(e)
            return
        }
      }
    }
    // TODO: obviously this is horrible, we'll fix when we switch to a DI strategy
    Singletons.history = state.history
  }

  protected def draw() {
    val columns = terminal.getWidth
    val screenRenderResult = ReplRenderer.render(state, terminalInfo)
    val previousScreenOpt = previousReplRenderResultOpt.map(_.screen)
    val output = screenRenderResult.screen.draw(previousScreenOpt, columns)
    previousReplRenderResultOpt = Some(screenRenderResult)

    System.out.write(output.getBytes)
    System.out.flush()
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
        e.printStackTrace()
        DebugLogger.logException(e)
    }
    if (state.continue)
      inputLoop()
    else {
      draw() // Tidy up before exiting
      println()
    }
  }

  private def fetchAction(): InputAction = {
    val isEmpty = state.lineBuffer.isEmpty
    state.mode match {
      case ReplMode.BrowseCompletions ⇒ InputAction.fetchAction(isEmpty, BrowseCompletionsKeyMap)
      case _                          ⇒ InputAction.fetchAction(isEmpty)
    }
  }

  private def handleAction(action: InputAction) = {
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
    val s = state.lineBuffer.s
    val pos = state.lineBuffer.cursorPos
    state.assistanceStateOpt =
      s match {
        case MishCommand(prefix, mishCmd) ⇒
          val newPos = pos - prefix.length // adjust for the prefix
          if (newPos >= 0)
            InvocationAssistance.getCallingSyntaxOfCurrentInvocation(s, newPos, getEnvironment, mish = true)
          else
            None
        case _ ⇒
          InvocationAssistance.getCallingSyntaxOfCurrentInvocation(s, pos, getEnvironment, mish = state.mish)
      }
  }

  protected def complete: Option[CompletionResult] = {
    val s = state.lineBuffer.s
    val pos = state.lineBuffer.cursorPos

    s match {
      case MishCommand(prefix, mishCmd) ⇒
        val shift = prefix.length // adjust for the prefix
        val newPos = pos - shift
        if (newPos >= 0)
          completer.complete(mishCmd, pos = newPos, getEnvironment, mish = true).map(_.translate(shift))
        else
          None
      case _ ⇒
        completer.complete(s, pos, getEnvironment, mish = state.mish)
    }
  }

  protected def getEnvironment: Environment = Environment(Map(), state.globalVariables)

  protected def terminalInfo = TerminalInfo(terminal.getHeight, terminal.getWidth)

}
