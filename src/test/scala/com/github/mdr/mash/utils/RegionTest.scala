package com.github.mdr.mash.utils

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class RegionTest extends FlatSpec with Matchers {

  "Detecting overlapping regions" should "work" in {
    Region(0, 10).overlaps(Region(20, 30)) should be(false)
    Region(20, 30).overlaps(Region(0, 10)) should be(false)
    Region(0, 10).overlaps(Region(0, 10)) should be(true)
    Region(0, 10).overlaps(Region(5, 15)) should be(true)
    Region(5, 15).overlaps(Region(0, 10)) should be(true)

    Region(0, 10).overlaps(Region(10, 20)) should be(false)
    Region(0, 11).overlaps(Region(10, 20)) should be(true)

    Region(0, 10).overlaps(Region(5, 0)) should be(true)
    Region(0, 10).overlaps(Region(0, 0)) should be(true)
  }

}