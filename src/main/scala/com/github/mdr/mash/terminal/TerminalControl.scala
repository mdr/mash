package com.github.mdr.mash.terminal

trait TerminalControl {

  def setEchoEnabled(enabled: Boolean)

  def configureTerminalForExternalProcess()

  def restore()
  
  def externalProcess[T](p: =>T): T = {
    configureTerminalForExternalProcess()
    try
      p
    finally
      restore()
  }
  
}

class TerminalControlImpl(terminal: jline.Terminal) extends TerminalControl {

  def setEchoEnabled(enabled: Boolean) {
    terminal.setEchoEnabled(enabled)
  }

  def configureTerminalForExternalProcess() {
    setEchoEnabled(true)
    TerminalHelper.stty(terminal, "sane")
  }

  def restore() {
    setEchoEnabled(false)
    TerminalHelper.stty(terminal, "-icanon min 1 -icrnl -inlcr -ixon")
  }

}
