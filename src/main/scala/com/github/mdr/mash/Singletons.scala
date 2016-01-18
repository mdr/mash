package com.github.mdr.mash

import com.github.mdr.mash.terminal.TerminalControl
import java.nio.file.Path

/**
 * Horrible global singletons, until we get the DI story sorted.
 */
object Singletons {

  var terminalControl: TerminalControl = _

  var workingDirectoryStack: WorkingDirectoryStack = new WorkingDirectoryStack

  var history: History = _
}