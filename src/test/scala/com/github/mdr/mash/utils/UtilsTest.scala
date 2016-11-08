package com.github.mdr.mash.utils

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.utils.Utils._

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

}