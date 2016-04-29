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

object Main extends App {

  if (args.size >= 2 && args(0) == "-c")
    runCommandViaSh(args(1))
  else
    launchRepl()

  private def launchRepl() = {
    handleSigint()
    TerminalHelper.withTerminal { terminal ⇒
      Singletons.terminalControl = new TerminalControlImpl(terminal)
      val repl = new Repl(new JLineTerminalWrapper(terminal), System.out, LinuxFileSystem, LinuxEnvironmentInteractions)
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

  private def handleSigint() {
    Signal.handle(new Signal("INT"), new SignalHandler() {
      override def handle(sig: Signal) {
        for (executionContext ← Singletons.executionContextOpt)
          executionContext.interrupt()
      }
    })
  }

}