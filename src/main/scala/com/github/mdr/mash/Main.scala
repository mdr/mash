package com.github.mdr.mash

import com.github.mdr.mash.terminal.TerminalControlImpl
import sun.misc.SignalHandler
import sun.misc.Signal
import com.github.mdr.mash.terminal.TerminalInfo
import com.github.mdr.mash.evaluator.Environment
import jline.Terminal
import scala.collection.JavaConverters._
import com.github.mdr.mash.terminal.JLineTerminalWrapper
import com.github.mdr.mash.repl.Repl
import com.github.mdr.mash.terminal.TerminalHelper

object Main extends App {

  if (args.size >= 2 && args(0) == "-c")
    runCommandViaSh(args(1))
  else
    launchRepl()

  private def launchRepl() = {
    handleSigint()
    TerminalHelper.withTerminal { terminal ⇒
      Singletons.terminalControl = new TerminalControlImpl(terminal)
      val repl = new Repl(new JLineTerminalWrapper(terminal), System.out)
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