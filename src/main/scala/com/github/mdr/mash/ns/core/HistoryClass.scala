package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.{ Field, MashClass }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.ns.time.DateTimeClass

object HistoryClass extends MashClass("core.History") {

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

  override def summaryOpt = Some("A record in Mash command history")

}