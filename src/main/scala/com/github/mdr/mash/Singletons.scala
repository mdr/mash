package com.github.mdr.mash

import com.github.mdr.mash.evaluator.ExecutionContext
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.repl.ScriptExecutor
import com.github.mdr.mash.repl.history.History
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.terminal.{ Terminal, TerminalControl }

/**
 * Horrible global singletons, until we get the DI story sorted.
 */
object Singletons {

  var scriptExecutor: ScriptExecutor = _

  var terminalControl: TerminalControl = _

  var workingDirectoryStack: WorkingDirectoryStack = new WorkingDirectoryStack(LinuxFileSystem.pwd)

  var history: History = _

  var _executionContextOpt: Option[ExecutionContext] = None

  def executionContextOpt: Option[ExecutionContext] = synchronized { _executionContextOpt }

  def setExecutionContext(ctx: ExecutionContext): Unit = synchronized { _executionContextOpt = Some(ctx) }

  var environment: MashObject = _

  var terminalWindowChanged = false

  var terminal: Terminal = _
}

object GlobalInterpreterLock {

  def withLock[T](f: ⇒ T): T = synchronized {
    f
  }

}
