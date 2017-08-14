package com.github.mdr.mash.terminal

import com.github.mdr.mash.utils.Dimensions

object DummyTerminal {

  val SufficientlyLargeTerminalSize = Dimensions(1000, 1000)

}

case class DummyTerminal(override val rows: Int = DummyTerminal.SufficientlyLargeTerminalSize.rows,
                         override val columns: Int = DummyTerminal.SufficientlyLargeTerminalSize.columns) extends Terminal {

  override def size = Dimensions(columns, rows)

}
