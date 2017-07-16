package com.github.mdr.mash.terminal

import com.github.mdr.mash.utils.Dimension

object Terminal {

  val ClearScreenEscapeSequence = "\u001b[H\u001b[2J"

}

trait Terminal {

  def size: Dimension

  def rows = size.rows

  def columns = size.columns

}

class JLineTerminalWrapper(terminal: jline.Terminal) extends Terminal {

  override def size = Dimension(terminal.getHeight, terminal.getWidth)

}

