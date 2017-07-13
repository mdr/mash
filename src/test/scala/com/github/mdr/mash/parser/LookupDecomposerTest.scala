package com.github.mdr.mash.parser

import com.github.mdr.mash.parser.LookupDecomposer._
import org.scalatest.{ FlatSpec, Matchers }

class LookupDecomposerTest extends FlatSpec with Matchers {

  "Lookup decomposer" should "decompose lookups with integer indices" in {

    decomposeLookupWithIntegerIndex("xs[10]") shouldEqual Some(LookupWithIntegerIndex("xs", 10))
    decomposeLookupWithIntegerIndex("xs[0]") shouldEqual Some(LookupWithIntegerIndex("xs", 0))
    decomposeLookupWithIntegerIndex("xs[1]") shouldEqual Some(LookupWithIntegerIndex("xs", 1))
    decomposeLookupWithIntegerIndex("foo.bar[10]") shouldEqual Some(LookupWithIntegerIndex("foo.bar", 10))

    decomposeLookupWithIntegerIndex("xs.foo") shouldEqual None
    decomposeLookupWithIntegerIndex("xs[1.2]") shouldEqual None

  }

  it should "decompose lookups with string indices" in {

    decomposeLookupWithStringIndex("obj['foo']") shouldEqual Some(LookupWithStringIndex("obj", "foo"))
    decomposeLookupWithStringIndex("obj['foo`nbar']") shouldEqual Some(LookupWithStringIndex("obj", "foo\nbar"))
    decomposeLookupWithStringIndex("""obj["foo"]""") shouldEqual Some(LookupWithStringIndex("obj", "foo"))

    decomposeLookupWithStringIndex("""obj[42]""") shouldEqual None
    decomposeLookupWithStringIndex("""obj.foo""") shouldEqual None

  }

  it should "decompose member expressions" in {

    decomposeMember("obj.foo") shouldEqual Some(LookupWithStringIndex("obj", "foo"))

    decomposeMember("obj.class") shouldEqual None
    decomposeMember("obj['foo']") shouldEqual None
    decomposeMember("""obj[42]""") shouldEqual None

  }

}
