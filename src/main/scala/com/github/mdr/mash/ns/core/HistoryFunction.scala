package com.github.mdr.mash.ns.core

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.functions.{ BoundParams, FunctionHelpers, MashFunction, ParameterModel }
import com.github.mdr.mash.repl.history.{ History, HistoryEntry }
import com.github.mdr.mash.runtime._

import scala.collection.immutable.ListMap

object HistoryFunction extends MashFunction("os.history") {

  private def history: History = Singletons.history

  val params = ParameterModel()

  override def call(boundParams: BoundParams): MashList =
    MashList(history.getHistory.reverse.map(asObject))

  private def asObject(entry: HistoryEntry): MashObject = {
    import HistoryEntryClass.Fields._
    MashObject.of(
      ListMap(
        Session -> entry.sessionIdOpt.map(id ⇒ MashString(id.toString)).getOrElse(MashNull),
        CommandNumber -> MashNumber(entry.commandNumber),
        Timestamp -> MashWrapped(entry.timestamp),
        Command -> MashString(entry.command),
        Mish -> MashBoolean(entry.mish),
        Result -> entry.resultOpt.getOrElse(MashNull),
        WorkingDirectory -> entry.workingDirectoryOpt.map(FunctionHelpers.asPathString).getOrElse(MashNull)),
      HistoryEntryClass)
  }

  override def typeInferenceStrategy = Seq(HistoryEntryClass)

  override def summaryOpt = Some("Return the command history as a sequence of History objects")

}