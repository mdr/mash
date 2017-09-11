package com.github.mdr.mash.screen

import com.github.mdr.mash.utils.CharUtils.Esc
import org.scalatest.{ FlatSpec, Matchers }
import com.github.mdr.mash.screen.Style._
import com.github.mdr.mash.utils.Point

class ScreenDrawerTest extends FlatSpec with Matchers {

  private val Title = "title"

  "ScreenDrawer" should "handle an empty screen" in {
    val screen = Screen(lines = Seq(), cursorPosOpt = None, title = Title)
    val ScreenDraw(drawString, None) = ScreenDrawer.draw(screen)
    replaceEscapes(drawString) shouldEqual s"(hide-cursor)(reset)(set-title $Title)"
  }

  it should "handle a single line" in {
    val line = Line("abc".style)
    val screen = Screen(lines = Seq(line), cursorPosOpt = Some(Point(0, 3)), title = Title)
    val ScreenDraw(drawString, None) = ScreenDrawer.draw(screen)
    replaceEscapes(drawString) shouldEqual
      s"(hide-cursor)(erase-line-from-cursor)(reset)(default-colour)abc(reset)(set-title $Title)(show-cursor)"
  }

  it should "handle a single extra character" in {
    val line1 = Line("abc".style)
    val screen1 = Screen(lines = Seq(line1), cursorPosOpt = Some(Point(0, 3)), title = Title)

    val line2 = Line("abcd".style)
    val screen2 = Screen(lines = Seq(line2), cursorPosOpt = Some(Point(0, 4)), title = Title)

    val ScreenDraw(drawString, None) = ScreenDrawer.draw(screen2, previousScreenOpt = Some(screen1))
    replaceEscapes(drawString) shouldEqual s"(hide-cursor)(reset)(erase-line-from-cursor)d(show-cursor)"
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
