package com.github.mdr.mash

import jline.UnixTerminal
import jline.Terminal
import jline.internal.TerminalLineSettings

object TerminalHelper {

  def withTerminal[T](p: Terminal ⇒ T): T = {
    val terminal = new UnixTerminal
    terminal.init()
    try
      p(terminal)
    finally
      terminal.restore()
  }

  def stty(terminal: Terminal, args: String) {
    // Call getSettings by reflection to support running from within sbt (because sbt uses an incompatible jline)
    val method = terminal.getClass.getDeclaredMethod("getSettings")
    method.setAccessible(true)
    val settings = method.invoke(terminal).asInstanceOf[TerminalLineSettings]
    //	val settings =  terminal.getSettings // <-- would prefer to use
    settings.set(args)
  }

}
