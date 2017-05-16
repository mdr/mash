package com.github.mdr.mash.printer

case class ColumnId(value: Int)

case class ColumnSpec(name: String, weight: Double = 1, isNullaryMethod: Boolean = false)

object ColumnAllocator {

  def allocateColumns(columnIds: Seq[ColumnId],
                      columnSpecs: Map[ColumnId, ColumnSpec],
                      requestedWidths: Map[ColumnId, Int],
                      availableWidth: Int): Map[ColumnId, Int] = {
    val satisfiedAllocations: Map[ColumnId, Int] =
      for {
        (columnId, allocated) ← allocateByWeight(columnIds, columnSpecs, availableWidth)
         requested = requestedWidths(columnId)
        if allocated >= requested
      } yield columnId -> requested

    val remainingWidth = availableWidth - satisfiedAllocations.values.sum
    val remainingColumnIds = columnIds.filterNot(satisfiedAllocations.contains)
    satisfiedAllocations ++ (
      if (remainingWidth < availableWidth)
        allocateColumns(remainingColumnIds, columnSpecs, requestedWidths, remainingWidth)
      else
        allocateByWeight(remainingColumnIds, columnSpecs, remainingWidth))
  }

  private def allocateByWeight(columnIds: Seq[ColumnId],
                               columnSpecs: Map[ColumnId, ColumnSpec],
                               availableWidth: Int): Map[ColumnId, Int] =
    if (columnIds.isEmpty)
      Map()
    else {
      val totalWeight = columnIds.map(id ⇒ columnSpecs(id).weight).sum
      var allocations: Map[ColumnId, Int] = {
        for (id ← columnIds)
          yield id -> (availableWidth * columnSpecs(id).weight / totalWeight).toInt
      }.toMap

      val excess = availableWidth - allocations.values.sum
      val (firstColumn, current) = allocations.head
      allocations += firstColumn -> (current + excess)
      allocations
    }

}