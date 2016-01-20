package com.github.mdr.mash.terminal

trait Terminal {

  def info: TerminalInfo

  def rows = info.rows

  def columns = info.columns

}

case class TerminalInfo(rows: Int, columns: Int)

class JLineTerminalWrapper(terminal: jline.Terminal) extends Terminal {

  override def info = TerminalInfo(terminal.getHeight, terminal.getWidth)

}

