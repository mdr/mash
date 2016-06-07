package com.github.mdr.mash.runtime

import java.time.LocalDate
import java.time.Instant

case class MashWrapped(x: Any) extends MashValue {

  require(x.isInstanceOf[Instant] || x.isInstanceOf[LocalDate], "Unexpected wrapped value	: " + x)

}
