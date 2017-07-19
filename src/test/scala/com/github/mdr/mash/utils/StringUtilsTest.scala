package com.github.mdr.mash.utils

import com.github.mdr.mash.utils.StringUtils.ellipsisise
import org.scalatest.{ FlatSpec, Matchers }

class StringUtilsTest extends FlatSpec with Matchers {

  "Ellipsisise" should "work" in {

    ellipsisise("123456", 3) should equal("12…")
    ellipsisise("123", 3) should equal("123")
    ellipsisise("12", 3) should equal("12")

    ellipsisise("1", 1) should equal("1")
    ellipsisise("12", 1) should equal("…")
    ellipsisise("1", 0) should equal("")
    ellipsisise("", 0) should equal("")

  }

}
