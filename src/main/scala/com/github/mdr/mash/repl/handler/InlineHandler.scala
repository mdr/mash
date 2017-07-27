package com.github.mdr.mash.repl.handler

import com.github.mdr.mash.commands.CommandRunner
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.language.ValueToExpression
import com.github.mdr.mash.repl.{ LineBuffer, Repl, ReplState }

import scala.util.control.NonFatal

trait InlineHandler {
  self: Repl ⇒

  protected def handleInline(state: ReplState): ReplState = {
    val cmd = state.lineBuffer.text
    if (cmd.trim.nonEmpty) {
      val commandRunner = new CommandRunner(output, terminal.size, globalVariables, sessionId)
      val unitName = s"command-inline"
      val unit = CompilationUnit(cmd, unitName, interactive = true, mish = state.mish)
      val resultOpt =
        try
          commandRunner.runCompilationUnit(unit, bareWords)
        catch {
          case NonFatal(e) ⇒
            debugLogger.logException(e)
            None
        }
      val updatedStateOpt =
        for {
          result ← resultOpt
          expression ← ValueToExpression.getExpression(result)
        } yield state.copy(lineBuffer = LineBuffer(expression))
      updatedStateOpt.getOrElse(state)
    } else
      state
  }

}
