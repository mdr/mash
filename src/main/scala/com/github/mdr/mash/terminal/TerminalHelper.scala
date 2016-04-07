package com.github.mdr.mash.terminal

import jline.UnixTerminal
import jline.internal.TerminalLineSettings

object TerminalHelper {

  def withTerminal[T](p: jline.Terminal ⇒ T): T = {
    val terminal = new UnixTerminal
    terminal.init()
    try
      p(terminal)
    finally
      terminal.restore()
  }

  def stty(terminal: jline.Terminal, args: String) {
    getSettings(terminal).set(args)
  }

  private def getSettings(terminal: jline.Terminal): TerminalLineSettings = {
    // We call getSettings by reflection to support running from within sbt (because sbt uses an incompatible 
    // conflicting version of jline)
    val method = terminal.getClass.getDeclaredMethod("getSettings")
    method.setAccessible(true)
    method.invoke(terminal).asInstanceOf[TerminalLineSettings]
    //	val settings =  terminal.getSettings // <-- would prefer to use
  }

}
