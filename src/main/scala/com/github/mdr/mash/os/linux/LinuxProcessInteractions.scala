package com.github.mdr.mash.os.linux

import com.github.mdr.mash.Posix
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.os.{ ProcessInfo, ProcessInteractions }
import com.jezhumble.javasysmon.{ JavaSysMon, ProcessInfo ⇒ JavaSysmonProcessInfo }

object LinuxProcessInteractions extends ProcessInteractions {

  val monitor = new JavaSysMon

  private def makeProcessInfo(info: JavaSysmonProcessInfo): ProcessInfo = {
    ProcessInfo(
      pid = info.getPid,
      parentPidOpt = if (info.getParentPid == 0) None else Some(info.getParentPid),
      name = info.getName,
      command = info.getCommand,
      owner = info.getOwner,
      residentSize = info.getResidentBytes,
      virtualSize = info.getTotalBytes)
  }

  def getProcesses: Seq[ProcessInfo] = monitor.processTable.map(makeProcessInfo)

  def getProcess(pid: Int): Option[ProcessInfo] =
    monitor.processTable.find(_.getPid == pid).map(makeProcessInfo)

  def kill(pid: Int, signal: Int) {
    val errorCode = Posix.posix.kill(pid, signal)
    //    val process = Runtime.getRuntime.exec(s"kill $pid")
    //    val errorCode = process.waitFor()
    if (errorCode > 0)
      throw new EvaluatorException(s"Unable to kill process with pid $pid, error code $errorCode")
  }

}