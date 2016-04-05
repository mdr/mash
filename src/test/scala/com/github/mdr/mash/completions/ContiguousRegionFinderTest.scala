package com.github.mdr.mash.completions

import org.scalatest._
import com.github.mdr.mash.utils.Region

class ContiguousRegionFinderTest extends FlatSpec with Matchers {

  check(
    "read",
    "  # ",
    "####")

  check(
    "the quick brown",
    "      ##       ",
    "    #####      ")

  check(
    "the 'quick brown' fox",
    "          #          ",
    "    #############    ")

  check(
    "/usr/local/bin",
    "             #",
    "##############")

  private def check(s: String, initialRegionPattern: String, expectedRegionPattern: String) {
    "ContiguousRegionFinder" should s"expand the region $initialRegionPattern to $expectedRegionPattern in $s" in {
      val initialRegion = region(initialRegionPattern)
      val expectedRegion = region(expectedRegionPattern)

      val actualRegion = ContiguousRegionFinder.getContiguousRegion(s, initialRegion, mish = false)
      actualRegion should equal(expectedRegion)
    }
  }

  private def region(pattern: String): Region = {
    val pos = pattern.indexOf("#")
    val length = pattern.filter(_ == '#').length
    Region(pos, length)
  }
}