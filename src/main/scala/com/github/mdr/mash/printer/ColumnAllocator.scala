package com.github.mdr.mash.printer

import com.github.mdr.mash.classes.Field
import com.github.mdr.mash.evaluator.{ Evaluator, MemberEvaluator, ToStringifier }
import com.github.mdr.mash.parser.ExpressionCombiner._
import com.github.mdr.mash.repl.browser.BrowserState.safeProperty
import com.github.mdr.mash.runtime.{ MashList, MashNumber, MashString, MashValue }
import com.github.mdr.mash.utils.Utils._

case class ColumnId(value: Int)

sealed trait ValueFetch {

  def lookup(value: MashValue): Option[MashValue]

  def name: String

  def value: MashValue

  def fetchPath(parentPath: String): String
}

object ValueFetch {

  object ByMember {

    def apply(field: Field): ByMember = ByMember(MashString(field.name))

    def apply(field: String): ByMember = ByMember(MashString(field))

  }

  case class ByMember(member: MashValue, isNullaryMethod: Boolean = false) extends ValueFetch {

    override def name = ToStringifier.stringify(member)

    def lookup(value: MashValue) =
      MemberEvaluator.maybeLookup(value, member).map(
        _.when(isNullaryMethod, rawValue ⇒ Evaluator.invokeNullaryFunctions(rawValue, locationOpt = None)))

    def fetchPath(parentPath: String): String = safeProperty(parentPath, member, isNullaryMethod)

    def value = member
  }

  case class ByIndex(index: Int) extends ValueFetch {

    def lookup(value: MashValue) = value match {
      case xs: MashList ⇒ xs.immutableElements.lift(index)
      case _            ⇒ None
    }

    def name = index.toString

    def fetchPath(parentPath: String): String = combineSafely(parentPath, s"[$index]")

    def value = MashNumber(index)
  }

}

case class ColumnSpec(fetch: ValueFetch, weight: Double = 1) {

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