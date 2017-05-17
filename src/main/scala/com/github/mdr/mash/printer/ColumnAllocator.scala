package com.github.mdr.mash.printer

import com.github.mdr.mash.classes.Field
import com.github.mdr.mash.evaluator.{ Evaluator, MemberEvaluator }
import com.github.mdr.mash.runtime.{ MashList, MashValue }
import com.github.mdr.mash.utils.Utils._

case class ColumnId(value: Int)

sealed trait ColumnFetch {

  def lookup(value: MashValue): Option[MashValue]

  def name: String

}

object ColumnFetch {

  object ByMember {

    def apply(field: Field): ByMember = ByMember(field.name)

  }

  case class ByMember(name: String, isNullaryMethod: Boolean = false) extends ColumnFetch {

    def lookup(value: MashValue) =
      MemberEvaluator.maybeLookup(value, name).map(
        _.when(isNullaryMethod, rawValue ⇒ Evaluator.invokeNullaryFunctions(rawValue, locationOpt = None)))

  }

  case class ByIndex(index: Int) extends ColumnFetch {

    def lookup(value: MashValue) = value match {
      case xs: MashList ⇒ xs.immutableElements.lift(index)
    }

    def name = index.toString

  }

}

case class ColumnSpec(fetch: ColumnFetch, weight: Double = 1) {

  def name = fetch.name

}

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