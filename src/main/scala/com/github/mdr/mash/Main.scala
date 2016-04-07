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

object Main extends App {

  Signal.handle(new Signal("INT"), new SignalHandler() {
    override def handle(sig: Signal) {
      for (executionContext ← Singletons.executionContextOpt)
        executionContext.interrupt()
    }
  })

  TerminalHelper.withTerminal { terminal ⇒
    Singletons.terminalControl = new TerminalControlImpl(terminal)
    if (args.size >= 2 && args(0) == "-c") {
      val command = args(1)
      //      DebugLogger.logMessage(s"Running command >>>$command<<<")
      val process = new ProcessBuilder(Seq("sh", "-c", command).asJava)
        .redirectInput(ProcessBuilder.Redirect.INHERIT)
        .redirectOutput(ProcessBuilder.Redirect.INHERIT)
        .redirectError(ProcessBuilder.Redirect.INHERIT).start()
      process.waitFor()
    } else {
      val repl = new Repl(new JLineTerminalWrapper(terminal), System.out)
      repl.run()
    }
  }

}