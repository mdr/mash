package com.github.mdr.mash.utils

import org.scalatest.{ FlatSpec, Matchers }

class RegionTest extends FlatSpec with Matchers {

  "Detecting overlapping regions" should "work" in {
    Region(0, length = 10).overlaps(Region(20, length = 30)) should be(false)
    Region(20, length = 30).overlaps(Region(0, length = 10)) should be(false)
    Region(0, length = 10).overlaps(Region(0, length = 10)) should be(true)
    Region(0, length = 10).overlaps(Region(5, length = 15)) should be(true)
    Region(5, length = 15).overlaps(Region(0, length = 10)) should be(true)

    Region(0, length = 10).overlaps(Region(10, length = 20)) should be(false)
    Region(0, length = 11).overlaps(Region(10, length = 20)) should be(true)

    Region(0, length = 10).overlaps(Region(5, length = 0)) should be(true)
    Region(0, length = 10).overlaps(Region(0, length = 0)) should be(true)
  }

  "Testing whether a region contains a position" should "work" in {
    Region(8, length = 2).contains(7) should be(false)
    Region(8, length = 2).contains(8) should be(true)
    Region(8, length = 2).contains(9) should be(true)
    Region(8, length = 2).contains(10) should be(false)

    Region(5, length = 0).contains(5) should be(false)
  }
}