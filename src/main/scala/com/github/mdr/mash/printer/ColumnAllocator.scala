package com.github.mdr.mash.printer

case class ColumnSpec(name: String, weight: Double = 1, isNullaryMethod: Boolean = false)

object ColumnAllocator {

  def allocateColumns(columns: Seq[ColumnSpec], requestedWidths: Map[ColumnSpec, Int], availableWidth: Int): Map[ColumnSpec, Int] = {
    val satisfiedAllocations: Map[ColumnSpec, Int] =
      for {
        (column, allocated) ← allocateByWeight(columns, availableWidth)
         requested = requestedWidths(column)
        if allocated >= requested
      } yield column -> requested

    val remainingWidth = availableWidth - satisfiedAllocations.values.sum
    val remainingColumns = columns.filterNot(satisfiedAllocations.contains)
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