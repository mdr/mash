package com.github.mdr.mash

import com.github.mdr.mash.repl.LineBuffer
import com.github.mdr.mash.utils.Utils

object LineBufferTestHelper {

  /**
   *  ▶ or ◀ points to the cursor position (and is removed from the string)
   */
  def parseLineBuffer(s: String): LineBuffer = {
    val pos = Utils.indexOf(s, "▶").orElse(Utils.indexOf(s, "◀").map(_ - 1)).getOrElse(
      throw new IllegalArgumentException("No cursor position provided in test case"))
    val text = s.filterNot(c ⇒ c == '▶' || c == '◀')
    LineBuffer(text, pos)
  }

}