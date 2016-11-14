package com.github.mdr.mash.runtime

import java.time.{ Instant, LocalDate }

case class MashWrapped(x: Any) extends MashValue {

  require(x.isInstanceOf[Instant] || x.isInstanceOf[LocalDate], "Unexpected wrapped value: " + (if (x == null) "null" else x.getClass))

  override def toString = x.toString
}
