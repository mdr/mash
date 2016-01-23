package com.github.mdr.mash.ns.core

import scala.collection.JavaConverters._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.FunctionHelpers._
import com.github.mdr.mash.inference._
import com.github.mdr.mash.os._
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.Singletons
import com.github.mdr.mash.History
import scala.collection.immutable.ListMap
import com.github.mdr.mash.HistoryEntry

object HistoryFunction extends MashFunction("os.history") {

  private def history: History = Singletons.history

  val params = ParameterModel()

  def apply(arguments: Arguments): MashList = {
    val boundParams = params.validate(arguments)
    MashList(history.getHistory.map(asObject))
  }

  private def asObject(entry: HistoryEntry): MashObject = {
    import HistoryClass.Fields._
    MashObject(ListMap(
      Timestamp -> entry.timestamp,
      Command -> MashString(entry.command),
      Mish -> entry.mish), HistoryClass)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Seq(Type.Instance(HistoryClass)))

  override def summary = "Return the command history as a sequence of History objects"

}