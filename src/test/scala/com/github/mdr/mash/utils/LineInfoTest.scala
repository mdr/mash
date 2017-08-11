package com.github.mdr.mash.utils

import org.scalatest.{ FlatSpec, Matchers }

class LineInfoTest extends FlatSpec with Matchers {

  "Line info" should "work" in {
    val s = """|line1
               |line2
               |line3""".stripMargin
    val info = new LineInfo(s)
    info.lineAndColumn(0) shouldEqual Point(0, 0)
    info.lineAndColumn(4) shouldEqual Point(0, 4)
    info.lineAndColumn(5) shouldEqual Point(0, 5)
    info.lineAndColumn(6) shouldEqual Point(1, 0)
    info.lineAndColumn(8) shouldEqual Point(1, 2)
    info.lineAndColumn(11) shouldEqual Point(1, 5)
    info.lineAndColumn(12) shouldEqual Point(2, 0)
    info.lineAndColumn(16) shouldEqual Point(2, 4)

    info.lines shouldEqual Seq("line1", "line2", "line3")

    info.lineStart(0) shouldEqual 0
    info.lineEnd(0) shouldEqual 6

    info.lineStart(1) shouldEqual 6
    info.lineEnd(1) shouldEqual 12

    info.lineStart(2) shouldEqual 12
    info.lineEnd(2) shouldEqual 17

    info.lineLength(0) shouldEqual 5

    info.lineRegions shouldEqual Seq(Region(0, 5), Region(6, 5), Region(12, 5))
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