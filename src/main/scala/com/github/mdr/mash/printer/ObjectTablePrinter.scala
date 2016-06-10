package com.github.mdr.mash.printer

import scala.collection.immutable.ListMap
import com.github.mdr.mash.evaluator.Evaluator
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.evaluator.MemberEvaluator
import com.github.mdr.mash.ns.collections.GroupClass
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.terminal.TerminalInfo
import java.io.PrintStream
import com.github.mdr.mash.ns.git.CommitClass

object ObjectTablePrinter {

  private val IndexColumnName = "#"

}

class ObjectTablePrinter(output: PrintStream, terminalInfo: TerminalInfo, alwaysUseBrowser: Boolean = false) {
  import ObjectTablePrinter._

  private val boxCharacterSupplier: BoxCharacterSupplier = UnicodeBoxCharacterSupplier
  private val renderer = new ObjectTableRenderer(terminalInfo)
  
  def printTable(objects: Seq[MashObject]): Option[ObjectTableModel] = {
    val model = renderer.renderObjects(objects)
    val nonDataRows = 3 /* header */ + 1 /* footer */
    if (alwaysUseBrowser || objects.size > terminalInfo.rows - nonDataRows)
      Some(model)
    else {
      printTable(model)
      None
    }
  }

  private def printTable(model: ObjectTableModel) {
    val ObjectTableModel(columnNames, columnWidths, objects) = model
    import boxCharacterSupplier._
    output.println(renderer.renderTopRow(model))
    output.println(renderer.renderHeaderRow(model))
    output.println(renderer.renderBelowHeaderRow(model))
    for (obj ← objects)
      output.println(renderer.renderObjectRow(model, obj))
    output.println(renderer.renderBottomRow(model))
  }

  private def getColumnSpecs(objects: Seq[MashObject]): Seq[ColumnSpec] = {
    val testObjects = objects.take(10)
    if (testObjects.forall(_.classOpt == Some(GroupClass)))
      Seq(
        ColumnSpec(GroupClass.Fields.Key.name, weight = 10),
        ColumnSpec(GroupClass.CountMethod.name, weight = 3, isNullaryMethod = true),
        ColumnSpec(GroupClass.Fields.Values.name, weight = 1))
    else if (testObjects.forall(_.classOpt == Some(CommitClass)))
      Seq(
        ColumnSpec(CommitClass.Fields.Hash.name, weight = 1),
        ColumnSpec(CommitClass.Fields.CommitTime.name, weight = 10),
        ColumnSpec(CommitClass.Fields.Author.name, weight = 10),
        ColumnSpec(CommitClass.Fields.Summary.name, weight = 3))
    else
      testObjects.flatMap(_.fields.keySet).distinct.map(field ⇒ ColumnSpec(field))
  }
}