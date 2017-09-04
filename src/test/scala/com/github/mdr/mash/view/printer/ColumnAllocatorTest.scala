package com.github.mdr.mash.view.printer

import org.scalatest.{ FlatSpec, Matchers }

class ColumnAllocatorTest extends FlatSpec with Matchers {

  "Allocating columns" should "work" in {
    val fooColumnId = ColumnId(0)
    val fooColumnSpec = ColumnSpec(ValueFetch.ByMember("foo"), 1)
    val allocatedColumns =
      ColumnAllocator.allocateColumns(
        columnIds = Seq(fooColumnId),
        columnSpecs = Map(fooColumnId -> fooColumnSpec),
        requestedWidths = Map(fooColumnId -> 5),
        availableWidth = 5)
    allocatedColumns should equal(Map(fooColumnId -> 5))
  }

  "Allocating columns" should "work 2" in {
    val fooColumnId = ColumnId(0)
    val barColumnId = ColumnId(1)
    val fooColumnSpec = ColumnSpec(ValueFetch.ByMember("foo"), 1)
    val barColumnSpec = ColumnSpec(ValueFetch.ByMember("bar"), 100)
    val allocatedColumns =
      ColumnAllocator.allocateColumns(
        columnIds = Seq(fooColumnId, barColumnId),
        columnSpecs = Map(fooColumnId -> fooColumnSpec, barColumnId -> barColumnSpec),
        requestedWidths = Map(fooColumnId -> 10, barColumnId -> 10),
        availableWidth = 15)
    allocatedColumns should equal(Map(fooColumnId -> 5, barColumnId -> 10))
  }

}