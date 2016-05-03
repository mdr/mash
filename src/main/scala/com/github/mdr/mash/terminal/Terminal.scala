package com.github.mdr.mash.terminal

object Terminal {
  
  val ClearScreenEscapeSequence = "\u001b[H\u001b[2J"
  
}

trait Terminal {

  def info: TerminalInfo

  def rows = info.rows

  def columns = info.columns

}

case class TerminalInfo(rows: Int, columns: Int)

class JLineTerminalWrapper(terminal: jline.Terminal) extends Terminal {

  override def info = TerminalInfo(terminal.getHeight, terminal.getWidth)

}

