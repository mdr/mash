package com.github.mdr.mash.printer

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import com.github.mdr.mash.parser.AbstractSyntax._
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.parser.{ ConcreteSyntax ⇒ Concrete }
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.utils.Utils.commonPrefix
import scala.collection.immutable.ListMap

class ColumnAllocatorTest extends FlatSpec with Matchers {

  "Allocating columns" should "work" in {
    val fooColumn = ColumnSpec("foo", 1)
    val barColumn = ColumnSpec("bar", 5)
    ColumnAllocator.allocateColumns(
      columns = Seq(fooColumn),
      requestedWidths = Map(fooColumn -> 5),
      availableWidth = 5) should equal(Map(fooColumn -> 5))
  }

  "Allocating columns" should "work2" in {
    val fooColumn = ColumnSpec("foo", 1)
    val barColumn = ColumnSpec("bar", 100)
    ColumnAllocator.allocateColumns(
      columns = Seq(fooColumn, barColumn),
      requestedWidths = Map(
        fooColumn -> 10,
        barColumn -> 10),
      availableWidth = 15) should equal(Map(fooColumn -> 5, barColumn -> 10))
  }

}