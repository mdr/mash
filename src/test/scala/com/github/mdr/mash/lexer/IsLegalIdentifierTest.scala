package com.github.mdr.mash.lexer

import com.github.mdr.mash.lexer.MashLexer.isLegalIdentifier
import org.scalatest.{ FlatSpec, Matchers }

class IsLegalIdentifierTest extends FlatSpec with Matchers {

  "Mash Lexer" should "identify legal identifiers" in {
    isLegalIdentifier("foo") shouldBe true
    isLegalIdentifier("x") shouldBe true
    isLegalIdentifier("x_") shouldBe true
    isLegalIdentifier("a1b2") shouldBe true

    isLegalIdentifier(".") shouldBe false
    isLegalIdentifier("class") shouldBe false
    isLegalIdentifier("true") shouldBe false
    isLegalIdentifier("null") shouldBe false
    isLegalIdentifier("123") shouldBe false
    isLegalIdentifier("foo-bar") shouldBe false
    isLegalIdentifier("_") shouldBe false
    isLegalIdentifier("_1") shouldBe false
  }

}
