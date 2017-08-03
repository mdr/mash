package com.github.mdr.mash.ns.core

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.functions.{ BoundParams, FunctionHelpers, MashFunction, ParameterModel }
import com.github.mdr.mash.repl.history.{ History, HistoryEntry }
import com.github.mdr.mash.runtime._

import scala.collection.immutable.ListMap

object HistoryFunction extends MashFunction("os.history") {

  private def history: History = Singletons.history

  val params = ParameterModel.Empty

  override def call(boundParams: BoundParams): MashList =
    MashList(history.getHistory.reverse.map(asObject))

  private def asObject(entry: HistoryEntry): MashObject = {
    import HistoryEntryClass.Fields._
    MashObject.of(
      ListMap(
        Session -> MashString(entry.sessionId.toString),
        CommandNumber -> MashNumber(entry.commandNumber),
        Timestamp -> MashWrapped(entry.timestamp),
        Command -> MashString(entry.command),
        Mish -> MashBoolean(entry.mish),
        Result -> entry.result,
        WorkingDirectory -> FunctionHelpers.asPathString(entry.workingDirectory)),
      HistoryEntryClass)
  }

  override def typeInferenceStrategy = Seq(HistoryEntryClass)

  override def summaryOpt = Some("Return the command history as a sequence of History objects")

}