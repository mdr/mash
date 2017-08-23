package com.github.mdr.mash.screen

object DefaultColours {

  // See: https://github.com/dracula/dracula-theme/

  private def fromHex(s: String) = Colour256.nearest(s) // RgbColour.parse(s)

  val Foreground = fromHex("#f8f8f2")
  val Comment = fromHex("#6272a4")
  val Cyan = fromHex("#8be9fd")
  val Green = fromHex("#50fa7b")
  val Orange = fromHex("#ffb86c")
  val Pink = fromHex("#ff79c6")
  val Purple = fromHex("#bd93f9")
  val Red = fromHex("#ff5555")
  val Yellow = fromHex("#f1fa8c")

}