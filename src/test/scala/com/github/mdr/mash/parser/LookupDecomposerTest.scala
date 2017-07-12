package com.github.mdr.mash.parser

import com.github.mdr.mash.parser.LookupDecomposer.{ NumericLookup, decomposeNumericLookup }
import org.scalatest.{ FlatSpec, Matchers }

class LookupDecomposerTest extends FlatSpec with Matchers {

  "Lookup decomposer" should "decompose numeric lookups" in {

    decomposeNumericLookup("xs[10]") shouldEqual Some(NumericLookup("xs", 10))
    decomposeNumericLookup("r0[0]") shouldEqual Some(NumericLookup("r0", 0))
    decomposeNumericLookup("r0[1]") shouldEqual Some(NumericLookup("r0", 1))
    decomposeNumericLookup("foo.bar[10]") shouldEqual Some(NumericLookup("foo.bar", 10))
    decomposeNumericLookup("xs.foo") shouldEqual None

  }

}
