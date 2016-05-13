package com.github.mdr.mash.utils

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.evaluator.MashNumber
import com.github.mdr.mash.parser.{ ConcreteSyntax ⇒ Concrete }
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.utils.Utils._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
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