package com.github.mdr.mash.repl

import com.github.mdr.mash.utils.Utils.indexOf

object LineBufferTestHelper {

  /**
    * ▶ or ◀ points to the cursor position (and is removed from the string)
    * » or « points to the selection offset (and is removed from the string)
    */
  def parseLineBuffer(s: String): LineBuffer = {
    val withoutSelectionMarkers = removeSelectionMarkers(s)
    val cursorOffset =
      indexOf(withoutSelectionMarkers, "▶").orElse(indexOf(withoutSelectionMarkers, "◀").map(_ - 1)).getOrElse(
        throw new IllegalArgumentException(s"No cursor position (▶ or ◀) provided in '$s'"))

    val withoutCursorMarkers = removeCursorMarkers(s)
    val selectionOffsetOpt = indexOf(withoutCursorMarkers, "»").orElse(indexOf(withoutCursorMarkers, "«").map(_ - 1))

    val text = removeCursorMarkers(withoutSelectionMarkers)
    LineBuffer(text, cursorOffset, selectionOffsetOpt)
  }

  private def removeCursorMarkers(s: String) = s.filterNot(c ⇒ c == '▶' || c == '◀')

  private def removeSelectionMarkers(s: String) = s.filterNot(c ⇒ c == '»' || c == '«')
}