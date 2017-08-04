package com.github.mdr.mash.utils

import org.scalatest.{ FlatSpec, Matchers }

class RegionTest extends FlatSpec with Matchers {

  "Detecting overlapping regions" should "work" in {
    Region(0, length = 10).overlaps(Region(20, length = 30)) shouldBe false
    Region(20, length = 30).overlaps(Region(0, length = 10)) shouldBe false
    Region(0, length = 10).overlaps(Region(0, length = 10)) shouldBe true
    Region(0, length = 10).overlaps(Region(5, length = 15)) shouldBe true
    Region(5, length = 15).overlaps(Region(0, length = 10)) shouldBe true

    Region(0, length = 10).overlaps(Region(10, length = 20)) shouldBe false
    Region(0, length = 11).overlaps(Region(10, length = 20)) shouldBe true

    Region(0, length = 10).overlaps(Region(5, length = 0)) shouldBe true
    Region(0, length = 10).overlaps(Region(0, length = 0)) shouldBe true
  }

  "Testing whether a region contains a position" should "work" in {
    Region(8, length = 2).contains(7) shouldBe false
    Region(8, length = 2).contains(8) shouldBe true
    Region(8, length = 2).contains(9) shouldBe true
    Region(8, length = 2).contains(10) shouldBe false

    Region(5, length = 0).contains(5) shouldBe false
  }

  "Testing whether a region contains another region" should "work" in {
    Region(10, 10) contains Region(14, 2) shouldBe true
    Region(10, 10) contains Region(10, 10) shouldBe true
    Region(10, 10) contains Region(10, 0) shouldBe true
    Region(10, 10) contains Region(19, 0) shouldBe true

    Region(10, 10) contains Region(0, 20) shouldBe false
    Region(10, 10) contains Region(0, 15) shouldBe false
    Region(10, 10) contains Region(15, 10) shouldBe false
    Region(10, 10) contains Region(20, 10) shouldBe false
    Region(10, 10) contains Region(20, 0) shouldBe false
    Region(10, 0) contains Region(10, 0) shouldBe false
    Region(10, 0) contains Region(10, 1) shouldBe false

  }

}