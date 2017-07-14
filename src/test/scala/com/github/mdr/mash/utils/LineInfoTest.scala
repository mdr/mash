package com.github.mdr.mash.utils

import com.github.mdr.mash.screen.Point
import org.scalatest.{ FlatSpec, Matchers }

class LineInfoTest extends FlatSpec with Matchers {

  "Line info" should "work" in {
    val s = """|line1
               |line2
               |line3""".stripMargin
    val info = new LineInfo(s)
    info.lineAndColumn(0) shouldEqual Point(0, 0)
    info.lineAndColumn(4) shouldEqual Point(0, 4)
    info.lineAndColumn(6) shouldEqual Point(1, 0)
    info.lineAndColumn(8) shouldEqual Point(1, 2)
    info.lineAndColumn(12) shouldEqual Point(2, 0)
    info.lineAndColumn(16) shouldEqual Point(2, 4)

    info.lines shouldEqual Seq("line1", "line2", "line3")
  }
  
  it should "work with an empty last line" in  {
    val s = """|abc
               |""".stripMargin
    val info = new LineInfo(s)
    info.lineAndColumn(4) shouldEqual Point(1, 0)
    info.lines shouldEqual Seq("abc", "")
    info.lineRegions shouldEqual Seq(Region(0, 3), Region(4, 0))
  }
  
}