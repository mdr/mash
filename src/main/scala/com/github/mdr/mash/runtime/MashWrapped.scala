package com.github.mdr.mash.runtime

import java.time.LocalDate
import java.time.Instant
import com.github.mdr.mash.utils.Utils

case class MashWrapped(x: Any) extends MashValue {

  require(x.isInstanceOf[Instant] || x.isInstanceOf[LocalDate], "Unexpected wrapped value	: " + x)

}
