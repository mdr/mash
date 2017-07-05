package com.github.mdr.mash.screen.browser

object ArrowHelper {

  def addUpArrow(s: String): String = setMiddleCharacter(s, '↑')

  def addDownArrow(s: String): String = setMiddleCharacter(s, '↓')

  private def setMiddleCharacter(s: String, c: Char): String =
    if (s.isEmpty) s else s.updated(s.length / 2, c)

}
