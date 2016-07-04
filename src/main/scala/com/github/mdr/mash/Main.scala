package com.github.mdr.mash

import scala.collection.JavaConverters._
import com.github.mdr.mash.os.linux.LinuxEnvironmentInteractions
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.repl.Repl
import com.github.mdr.mash.terminal.JLineTerminalWrapper
import com.github.mdr.mash.terminal.TerminalControlImpl
import com.github.mdr.mash.terminal.TerminalHelper
import sun.misc.Signal
import sun.misc.SignalHandler
import com.github.mdr.mash.repl.history.History
import com.github.mdr.mash.repl.history.HistoryImpl
import com.github.mdr.mash.repl.history.FileBackedHistoryStorage
import scala.language.implicitConversions
import java.util.UUID

object Main extends App {

  if (args.size >= 2 && args(0) == "-c")
    runCommandViaSh(args(1))
  else
    launchRepl()

  private def launchRepl() = {
    handleSignals()
    TerminalHelper.withTerminal { terminal ⇒
      // TODO: obviously this is horrible, will be fixed when DI gets sorted out
      Singletons.terminalControl = new TerminalControlImpl(terminal)
      Singletons.history = new HistoryImpl(new FileBackedHistoryStorage, sessionId = UUID.randomUUID)

      val repl = new Repl(new JLineTerminalWrapper(terminal), System.out, LinuxFileSystem, LinuxEnvironmentInteractions, history = Singletons.history)
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

  implicit def signalHandler(f: Signal ⇒ Unit): SignalHandler = new SignalHandler() {
    override def handle(sig: Signal) = f(sig)
  }

  private def handleSignals() {
    Signal.handle(new Signal("INT"), handleInterrupt _)
    Signal.handle(new Signal("WINCH"), handleWindowChange _)
  }

  private def handleInterrupt(sig: Signal) {
    for (executionContext ← Singletons.executionContextOpt)
      executionContext.interrupt()
  }
  private def handleWindowChange(sig: Signal) {
    Singletons.terminalWindowChanged = true
  }
}