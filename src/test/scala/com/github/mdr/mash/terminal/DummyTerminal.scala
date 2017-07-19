package com.github.mdr.mash.terminal

import com.github.mdr.mash.utils.Dimensions

case class DummyTerminal(width: Int = 80) extends Terminal {

  override def size = Dimensions(width, 40)

}
