package com.github.mdr.mash.utils

import org.scalatest.{ FlatSpec, Matchers }

class PrefixTreeTest extends FlatSpec with Matchers {

  "Prefix tree" should "work with a single item" in {
    val tree = PrefixTree("foo" -> 42)
    tree.get("foo") should equal(Some(42))
    tree.get("bar") should equal(None)
    tree.get("fo") should equal(None)
  }

  "Prefix tree" should "work with multiple items" in {
    val tree = PrefixTree(
      "bar" -> 42,
      "baz" -> 128,
      "bibble" -> 100)
    tree.get("bar") should equal(Some(42))
  }

}