package com.github.mdr.mash.repl

import com.github.mdr.mash.utils.Utils

object LineBufferTestHelper {

  /**
   *  ▶ or ◀ points to the cursor position (and is removed from the string)
   */
  def parseLineBuffer(s: String): LineBuffer = {
    val pos = Utils.indexOf(s, "▶").orElse(Utils.indexOf(s, "◀").map(_ - 1)).getOrElse(
      throw new IllegalArgumentException(s"No cursor position (▶ or ◀) provided in '$s'"))
    val text = s.filterNot(c ⇒ c == '▶' || c == '◀')
    LineBuffer(text, pos)
  }

}