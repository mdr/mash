package com.github.mdr.mash.printer

import scala.collection.immutable.ListMap

case class ColumnSpec(name: String, weight: Double = 1, isNullaryMethod: Boolean = false)

object ColumnAllocator {

  def allocateColumns(columns: Seq[ColumnSpec], requestedWidths: Map[ColumnSpec, Int], availableWidth: Int): Map[ColumnSpec, Int] = {
    val totalWeight = columns.map(_.weight).sum
    var satisfiedAllocations: Map[ColumnSpec, Int] =
      for {
        (c, allocation) ← allocateByWeight(columns, availableWidth)
        if allocation - requestedWidths(c) >= 0
      } yield c -> requestedWidths(c)

    val remainingWidth = availableWidth - satisfiedAllocations.values.sum
    val remainingColumns = columns.filterNot(c ⇒ satisfiedAllocations.contains(c))
    satisfiedAllocations ++ (
      if (remainingWidth < availableWidth)
        allocateColumns(remainingColumns, requestedWidths, remainingWidth)
      else
        allocateByWeight(remainingColumns, remainingWidth))
  }

  private def allocateByWeight(columns: Seq[ColumnSpec], availableWidth: Int): Map[ColumnSpec, Int] =
    if (columns.isEmpty)
      Map()
    else {
      val totalWeight = columns.map(_.weight).sum
      var allocations: Map[ColumnSpec, Int] = {
        for (c ← columns)
          yield c -> (availableWidth * c.weight / totalWeight).toInt
      }.toMap

      val excess = availableWidth - allocations.values.sum
      val (firstColumn, current) = allocations.head
      allocations += firstColumn -> (current + excess)
      allocations
    }

}