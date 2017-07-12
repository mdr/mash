package com.github.mdr.mash.parser

import com.github.mdr.mash.parser.LookupDecomposer.{ NumericLookup, decomposeNumericLookup }
import org.scalatest.{ FlatSpec, Matchers }

class LookupDecomposerTest extends FlatSpec with Matchers {

  "Lookup decomposer" should "decompose numeric lookups" in {

    decomposeNumericLookup("xs[10]") shouldEqual Some(NumericLookup("xs", 10))
    decomposeNumericLookup("xs[0]") shouldEqual Some(NumericLookup("xs", 0))
    decomposeNumericLookup("xs[1]") shouldEqual Some(NumericLookup("xs", 1))
    decomposeNumericLookup("foo.bar[10]") shouldEqual Some(NumericLookup("foo.bar", 10))

    decomposeNumericLookup("xs.foo") shouldEqual None
    decomposeNumericLookup("xs[1.2]") shouldEqual None

  }

}
