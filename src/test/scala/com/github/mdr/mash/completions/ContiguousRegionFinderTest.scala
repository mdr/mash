package com.github.mdr.mash.completions

import com.github.mdr.mash.utils.Region
import org.scalatest._

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

  check(
    "foo bar baz",
    "    #      ",
    "    ###    ")

  check(
    "(ls)",
    " #  ",
    " ## ",
    liberal = false)

  check(
    "(/App)",
    " #    ",
    " #### ",
    liberal = false)

  private def check(s: String, initialRegionPattern: String, expectedRegionPattern: String, liberal: Boolean = true) {
    "ContiguousRegionFinder" should s"expand the region $initialRegionPattern to $expectedRegionPattern in $s" in {
      val initialRegion = region(initialRegionPattern)
      val expectedRegion = region(expectedRegionPattern)

      val actualRegion = ContiguousRegionFinder.getContiguousRegion(s, initialRegion, mish = false, liberal = liberal)
      actualRegion should equal(expectedRegion)
    }
  }

  private def region(pattern: String): Region = {
    val pos = pattern.indexOf("#")
    val length = pattern.filter(_ == '#').length
    Region(pos, length)
  }
}