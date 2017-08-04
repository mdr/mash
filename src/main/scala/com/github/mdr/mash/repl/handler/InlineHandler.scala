package com.github.mdr.mash.repl.handler

import com.github.mdr.mash.commands.CommandRunner
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.language.ValueToExpression
import com.github.mdr.mash.repl.{ LineBuffer, Repl, ReplState }
import com.github.mdr.mash.runtime.MashValue

import scala.util.control.NonFatal

trait InlineHandler {
  self: Repl ⇒

  protected def handleInline(state: ReplState): ReplState = {
    val lineBuffer = state.lineBuffer
    val cmd = lineBuffer.selectedTextOpt getOrElse lineBuffer.text
    if (cmd.trim.nonEmpty) {
      val updatedStateOpt =
        for {
          result ← runCommand(cmd, state)
          expression ← ValueToExpression.getExpression(result)
          newLineBuffer = if (lineBuffer.hasSelection) lineBuffer.replaceRegion(lineBuffer.selectedRegion, expression) else LineBuffer(expression)
        } yield state.copy(lineBuffer = newLineBuffer)
      updatedStateOpt.getOrElse(state)
    } else
      state
  }

  private def runCommand(cmd: String, state: ReplState): Option[MashValue] = {
    val commandRunner = new CommandRunner(output, terminal.size, globalVariables, sessionId, printErrors = false)
    val unitName = s"command-inline"
    val unit = CompilationUnit(cmd, unitName, interactive = true, mish = state.mish)
    try
      commandRunner.runCompilationUnit(unit, bareWords)
    catch {
      case NonFatal(e) ⇒
        debugLogger.logException(e)
        None
    }
  }
}
