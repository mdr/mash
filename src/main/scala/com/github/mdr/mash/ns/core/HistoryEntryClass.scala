package com.github.mdr.mash.ns.core

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.evaluator.{ Arguments, EvaluatorException, Field, MashClass }
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.runtime.{ MashBoolean, MashObject, MashString, MashValue }

object HistoryEntryClass extends MashClass("core.HistoryEntry") {

  private lazy val scriptExecutor = Singletons.scriptExecutor

  object Fields {
    val Session = Field("session", Some("ID of shell session"), StringClass)
    val CommandNumber = Field("commandNumber", Some("Number of the command within the session"), NumberClass)
    val Timestamp = Field("timestamp", Some("Time command was executed"), DateTimeClass)
    val Command = Field("command", Some("Command"), StringClass)
    val Mish = Field("mish", Some("Whether the command was executed in mish mode"), BooleanClass)
    val Result = Field("result", Some("Result of the command (if available, else null)"), Type.Any)
    val WorkingDirectory = Field("workingDirectory", Some("Directory where the command was executed"), StringClass taggedWith PathClass)
  }

  import Fields._

  override val fields = Seq(Session, CommandNumber, Timestamp, Command, Mish, Result, WorkingDirectory)

  override val methods = Seq(
    ReexecuteMethod)

  object ReexecuteMethod extends MashMethod("reexecute") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      val entryObject = target.asInstanceOf[MashObject]
      val command = entryObject.get(Command).getOrElse(
        throw new EvaluatorException("Invalid history entry")).asInstanceOf[MashString].s
      val mish = entryObject.get(Mish).get.asInstanceOf[MashBoolean].value
      scriptExecutor.runUnit(CompilationUnit(command, "eval", mish = mish))
    }

    override def summaryOpt = Some("Re-execute the command ran in this history entry")

  }

  override def summaryOpt = Some("An entry in Mash command history")

}