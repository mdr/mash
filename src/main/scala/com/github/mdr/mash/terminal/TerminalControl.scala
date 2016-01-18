package com.github.mdr.mash.terminal

import jline.Terminal
import com.github.mdr.mash.TerminalHelper

trait TerminalControl {

  def setEchoEnabled(enabled: Boolean)

  def configureTerminalForExternalProcess()

  def restore()
}

class TerminalControlImpl(terminal: Terminal) extends TerminalControl {

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
