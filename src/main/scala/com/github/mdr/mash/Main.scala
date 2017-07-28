package com.github.mdr.mash

import java.util.UUID

import com.github.mdr.mash.evaluator.StandardEnvironment
import com.github.mdr.mash.os.linux.{ LinuxEnvironmentInteractions, LinuxFileSystem }
import com.github.mdr.mash.repl.history.{ FileBackedHistoryStorage, HistoryImpl }
import com.github.mdr.mash.repl.{ Repl, ScriptExecutor }
import com.github.mdr.mash.terminal.{ JLineTerminalWrapper, TerminalControlImpl, TerminalHelper }
import sun.misc.Signal

import scala.collection.JavaConverters._
import scala.language.implicitConversions

object Main extends App {

  if (args.size >= 2 && args(0) == "-c")
    runCommandViaSh(args(1))
  else
    launchRepl()

  private def launchRepl() = {
    handleSignals()
    val sessionId = UUID.randomUUID
    TerminalHelper.withTerminal { terminal ⇒
      // TODO: obviously this is horrible, will be fixed when DI gets sorted out
      val output = System.out
      val terminalWrapper = new JLineTerminalWrapper(terminal)
      val globalVariables = StandardEnvironment.createGlobalVariables()
      val ns = globalVariables(StandardEnvironment.Ns).asObject.getOrElse(throw new AssertionError("ns was not an object"))
      Singletons.terminalControl = new TerminalControlImpl(terminal)
      Singletons.history = new HistoryImpl(new FileBackedHistoryStorage, sessionId = sessionId)
      Singletons.scriptExecutor = new ScriptExecutor(output, terminalWrapper, sessionId, globalVariables)

      val loader = new Loader(terminalWrapper, output, sessionId, globalVariables, ns)
      loader.load()

      val initScriptRunner = new InitScriptRunner(terminalWrapper, output, sessionId, globalVariables)
      initScriptRunner.processInitFile()

      val repl = new Repl(terminalWrapper, output, LinuxFileSystem, LinuxEnvironmentInteractions,
        history = Singletons.history, sessionId = sessionId, globalVariables = globalVariables)
      repl.run()
    }
  }

  private def runCommandViaSh(command: String) {
    val process = new ProcessBuilder(Seq("sh", "-c", command).asJava)
      .redirectInput(ProcessBuilder.Redirect.INHERIT)
      .redirectOutput(ProcessBuilder.Redirect.INHERIT)
      .redirectError(ProcessBuilder.Redirect.INHERIT).start()
    process.waitFor()
  }

  private def handleSignals() {
    Signal.handle(new Signal("INT"), handleInterrupt)
    Signal.handle(new Signal("WINCH"), handleWindowChange)
  }

  private def handleInterrupt(sig: Signal) {
    for (executionContext ← Singletons.executionContextOpt)
      executionContext.interrupt()
  }

  private def handleWindowChange(sig: Signal) {
    Singletons.terminalWindowChanged = true
  }
}