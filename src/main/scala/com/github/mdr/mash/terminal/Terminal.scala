package com.github.mdr.mash.terminal

import com.github.mdr.mash.utils.Dimensions

trait Terminal {

  def size: Dimensions

  def rows = size.rows

  def columns = size.columns

}

class JLineTerminalWrapper(terminal: jline.Terminal) extends Terminal {

  override def size = Dimensions(terminal.getHeight, terminal.getWidth)

}

