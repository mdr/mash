package com.github.mdr.mash

import com.github.mdr.mash.terminal.TerminalControl
import java.nio.file.Path
import com.github.mdr.mash.evaluator.ExecutionContext

/**
 * Horrible global singletons, until we get the DI story sorted.
 */
object Singletons {

  var terminalControl: TerminalControl = _

  var workingDirectoryStack: WorkingDirectoryStack = new WorkingDirectoryStack

  var history: History = _

  var _executionContextOpt: Option[ExecutionContext] = None

  def executionContextOpt: Option[ExecutionContext] = synchronized { _executionContextOpt }

  def setExecutionContext(ctx: ExecutionContext): Unit = synchronized { _executionContextOpt = Some(ctx) }

}