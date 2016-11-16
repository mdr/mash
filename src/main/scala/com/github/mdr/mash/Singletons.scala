package com.github.mdr.mash

import com.github.mdr.mash.evaluator.ExecutionContext
import com.github.mdr.mash.repl.history.History
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.terminal.TerminalControl

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

  var environment: MashObject = _

  var terminalWindowChanged = false
}

object GlobalInterpreterLock {

  def withLock[T](f: => T): T = synchronized {
    f
  }

}
