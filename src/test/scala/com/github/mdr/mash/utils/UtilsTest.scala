package com.github.mdr.mash.utils

import com.github.mdr.mash.utils.Utils._
import org.scalatest.{ FlatSpec, Matchers }

class UtilsTest extends FlatSpec with Matchers {

  "Common prefix" should "work" in {
    commonPrefix("", "") should equal("".toSeq)
    commonPrefix("a", "") should equal("".toSeq)
    commonPrefix("", "a") should equal("".toSeq)
    commonPrefix("a", "b") should equal("".toSeq)
    commonPrefix("abc", "abd") should equal("ab".toSeq)
  }

  "Intercalate" should "intersperse sequences within a sequence of sequences" in {
    intercalate(Seq[Seq[Char]]("foo", "bar", "baz"), ":::") should equal("foo:::bar:::baz".toSeq)
  }

  "truncate" should "work" in {
    Utils.truncate(Seq(1, 2, 3, 4, 5), 3, 0) should equal(Seq(1, 2, 0))
  }

}