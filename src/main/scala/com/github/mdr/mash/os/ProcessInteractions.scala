package com.github.mdr.mash.os

import com.jezhumble.javasysmon.JavaSysMon
import com.jezhumble.javasysmon.{ ProcessInfo ⇒ JavaSysmonProcessInfo }
import com.github.mdr.mash.evaluator.EvaluatorException

trait ProcessInteractions {

  def kill(pid: Int, signal: Int)

  def getProcess(pid: Int): Option[ProcessInfo]

  def getProcesses: Seq[ProcessInfo]
}

case class ProcessInfo(
  pid: Int,
  parentPidOpt: Option[Int],
  name: String,
  command: String,
  owner: String,
  residentSize: Long,
  virtualSize: Long)

