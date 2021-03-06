package com.github.mdr.mash.utils

import com.github.mdr.mash.screen.{ StyledCharacter, StyledString }
import com.github.mdr.mash.screen.Style._

object StyledStringUtils {

  val Ellipsis = '…'.style

  /**
    * Ensure the given string is at most maxLength characters long (including 0), by truncating and adding …
    */
  def ellipsisise(s: StyledString, maxLength: Int, ellipsis: StyledCharacter = Ellipsis): StyledString = {
    require(maxLength >= 0, s"maxLength must be non-negative, but was $maxLength")
    if (maxLength == 0)
      StyledString.Empty
    else {
      val excess = s.length - maxLength
      if (excess <= 0)
        s
      else
        s.take(maxLength - 1) + ellipsis.asString
    }
  }
}
