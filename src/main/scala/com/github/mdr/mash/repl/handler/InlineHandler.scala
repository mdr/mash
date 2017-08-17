package com.github.mdr.mash.repl.handler

import com.github.mdr.mash.commands.CommandRunner
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.language.ValueToExpression
import com.github.mdr.mash.repl.{ LineBuffer, Repl, ReplState }
import com.github.mdr.mash.runtime.MashValue

import scala.util.control.NonFatal

trait InlineHandler {
  self: Repl ⇒

  protected def handleInline(state: ReplState): ReplState =
    state.withLineBuffer(handleInline(state.lineBuffer, state.mish))

  protected def handleInline(lineBuffer: LineBuffer, mish: Boolean): LineBuffer = {
    val cmd = lineBuffer.selectedTextOpt getOrElse lineBuffer.text
    val newLineBufferOpt =
      for {
        result ← runCommand(cmd, mish)
        expression ← ValueToExpression.getExpression(result)
        newLineBuffer = lineBuffer.selectedRegionOpt match {
          case Some(selectedRegion) ⇒ lineBuffer.replaceRegion(selectedRegion, expression)
          case None                 ⇒ LineBuffer(expression)
        }
      } yield newLineBuffer
    newLineBufferOpt getOrElse lineBuffer
  }

  private def runCommand(cmd: String, mish: Boolean = false): Option[MashValue] = {
    val commandRunner = new CommandRunner(output, terminal.size, globalVariables, sessionId, printErrors = false)
    val unitName = s"command-inline"
    val unit = CompilationUnit(cmd, unitName, interactive = true, mish = mish)
    try
      commandRunner.runCompilationUnit(unit, bareWords)
    catch {
      case NonFatal(e) ⇒
        debugLogger.logException(e)
        None
    }
  }
}
