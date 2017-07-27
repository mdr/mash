package com.github.mdr.mash.repl

import java.io.PrintStream
import java.util.UUID

import com.github.mdr.mash.commands.CommandRunner
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.runtime._
import com.github.mdr.mash.terminal.Terminal
import com.github.mdr.mash.{ ConfigWrapper, DebugLogger }

import scala.util.control.NonFatal

class ScriptExecutor(output: PrintStream, terminal: Terminal, sessionId: UUID, globalVariables: MashObject) {
  private val debugLogger = new DebugLogger(sessionId.toString)

  def runUnit(unit: CompilationUnit): MashValue = {
    val commandRunner = new CommandRunner(output, terminal.size, globalVariables, sessionId)
    val config = ConfigWrapper.fromGlobals(globalVariables)
    try
      commandRunner.runCompilationUnit(unit, config.bareWords).getOrElse(MashUnit)
    catch {
      case NonFatal(e) ⇒
        output.println(s"Unexpected error executing ${unit.name}")
        e.printStackTrace(output)
        debugLogger.logException(e)
        MashUnit
    }
  }
}