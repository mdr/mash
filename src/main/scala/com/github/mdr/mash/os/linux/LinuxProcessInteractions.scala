package com.github.mdr.mash.os.linux

import com.github.mdr.mash.Posix
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.os.{ ProcessInfo, ProcessInteractions }
import oshi.SystemInfo
import oshi.software.os.OSProcess

import scala.collection.JavaConverters._

object LinuxProcessInteractions extends ProcessInteractions {

  private lazy val operatingSystem = new SystemInfo().getOperatingSystem

  private def makeProcessInfo(process: OSProcess): ProcessInfo = {
    val command = process.getCommandLine
    ProcessInfo(
      pid = process.getProcessID,
      parentPidOpt = if (process.getParentProcessID == 0) None else Some(process.getParentProcessID),
      name = process.getName,
      command = if (command.isEmpty) process.getName else command,
      owner = process.getUser,
      residentSize = process.getResidentSetSize,
      virtualSize = process.getVirtualSize)
  }

  def getProcesses: Seq[ProcessInfo] = operatingSystem.getProcesses.asScala.map(makeProcessInfo)

  def getProcess(pid: Int): Option[ProcessInfo] =
    Option(operatingSystem.getProcess(pid)).map(makeProcessInfo)

  def kill(pid: Int, signal: Int) {
    val errorCode = Posix.posix.kill(pid, signal)
    //    val process = Runtime.getRuntime.exec(s"kill $pid")
    //    val errorCode = process.waitFor()
    if (errorCode != 0)
      throw EvaluatorException(s"Unable to kill process with pid $pid, error code $errorCode")
  }

}
