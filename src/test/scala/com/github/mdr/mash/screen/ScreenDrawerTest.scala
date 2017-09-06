package com.github.mdr.mash.screen

import com.github.mdr.mash.utils.CharUtils.Esc
import org.scalatest.{ FlatSpec, Matchers }
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.utils.Point

class ScreenDrawerTest extends FlatSpec with Matchers {

  "ScreenDrawer" should "handle an empty screen" in {
    val screen = Screen(lines = Seq(), cursorPosOpt = None, title = "mash")
    val ScreenDraw(drawString, None) = ScreenDrawer.draw(screen)
    replaceEscapes(drawString) shouldEqual "(hide-cursor)(set-title mash)"
  }

  it should "handle a single line" in {
    val line = Line("abc".style)
    val screen = Screen(lines = Seq(line), cursorPosOpt = Some(Point(0, 3)), title = "mash")
    val ScreenDraw(drawString, None) = ScreenDrawer.draw(screen)
    replaceEscapes(drawString) shouldEqual "(hide-cursor)(erase-line-from-cursor)(reset)(default-colour)abc(reset)(set-title mash)(show-cursor)"
  }

  private def replaceEscapes(s: String): String = {
    val replacements = Map(
      s"$Esc\\[\\?25l" → "(hide-cursor)",
      s"$Esc\\[\\?25h" → "(show-cursor)",
      s"$Esc\\[39;49m" → "(default-colour)",
      s"$Esc\\[0m" → "(reset)",
      s"$Esc\\[0?K" → "(erase-line-from-cursor)",
      s"$Esc]0;(.+?)\u0007" → "(set-title $1)")
    var result = s
    for ((pattern, replacement) ← replacements)
      result = result.replaceAll(pattern, replacement)
    result.replaceAll(Esc.toString, "(esc)")
  }

}
