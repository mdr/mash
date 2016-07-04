package com.github.mdr.mash.ns.os

import scala.collection.immutable.ListMap
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.os.ProcessInfo
import com.github.mdr.mash.os.linux.LinuxProcessInteractions
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashNumber
import com.github.mdr.mash.runtime.MashList
import com.github.mdr.mash.runtime.MashNull
import com.github.mdr.mash.runtime.MashValue
import com.github.mdr.mash.runtime.MashUnit

object ProcessClass extends MashClass("os.Process") {

  private val processInteractions = LinuxProcessInteractions

  object Fields {
    val Pid = Field("pid", "Id of process", Type.Tagged(NumberClass, PidClass))
    val ParentPid = Field("parentPid", "Id of parent process", Type.Tagged(NumberClass, PidClass))
    val Name = Field("name", "Name", Type.Instance(StringClass))
    val Command = Field("command", "Command", Type.Instance(StringClass))
    val Owner = Field("owner", "Owner", Type.Tagged(StringClass, UsernameClass))
    val ResidentSize = Field("residentSize", "Resident set size (RSS) in bytes", Type.Tagged(NumberClass, BytesClass))
    val VirtualSize = Field("virtualSize", "Virtual memory size (VSZ) in bytes", Type.Tagged(NumberClass, BytesClass))
  }

  import Fields._

  override val fields = Seq(Pid, ParentPid, Name, Command, Owner, ResidentSize, VirtualSize)

  override val methods = Seq(
    ChildrenMethod,
    KillMethod,
    ParentMethod)

  case class Wrapper(target: MashValue) {

    private val obj = target.asInstanceOf[MashObject]

    def pid: Int = obj(Pid).asInstanceOf[MashNumber].asInt.get

    def parentPidOpt: Option[Int] =
      MashNull.option(obj(ParentPid)).map(_.asInstanceOf[MashNumber]).flatMap(_.asInt)

  }

  def makeProcess(info: ProcessInfo): MashObject =
    MashObject.of(
      ListMap(
        Pid -> MashNumber(info.pid, PidClass),
        ParentPid -> info.parentPidOpt.map(pid ⇒ MashNumber(pid, PidClass)).getOrElse(MashNull),
        Name -> MashString(info.name),
        Command -> MashString(info.command),
        Owner -> MashString(info.owner, UsernameClass),
        ResidentSize -> MashNumber(info.residentSize, BytesClass),
        VirtualSize -> MashNumber(info.virtualSize, BytesClass)),
      ProcessClass)

  object KillMethod extends MashMethod("kill") {

    object Params {
      val Signal = KillFunction.Params.Signal.copy(isFlag = false)
    }
    import Params._

    val params = ParameterModel(Seq(Signal))

    def apply(target: MashValue, arguments: Arguments): MashUnit = {
      val boundParams = params.validate(arguments)
      val pid = Wrapper(target).pid
      val signal = KillFunction.getSignal(boundParams, Params.Signal)
      processInteractions.kill(pid, signal)
      MashUnit
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Unit)

    override def summary = "Kill this process"

    override def descriptionOpt = Some("""Examples:
  process.kill "HUP" # Send the HUP signal the the process""")

  }

  object ParentMethod extends MashMethod("parent") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      val parentPidOpt = Wrapper(target).parentPidOpt
      val parentProcessOpt =
        for {
          parentPid ← parentPidOpt
          processInfo ← processInteractions.getProcess(parentPid)
        } yield makeProcess(processInfo)
      parentProcessOpt.getOrElse(MashNull)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(ProcessClass)

    override def summary = "The parent process"

  }

  object ChildrenMethod extends MashMethod("children") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashList = {
      params.validate(arguments)
      val pid = Wrapper(target).pid
      val children = processInteractions.getProcesses.filter(_.parentPidOpt == Some(pid))
      MashList(children.map(makeProcess))
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Seq(Type.Instance(ProcessClass)))

    override def summary = "Children of this process"

  }

  override def summary = "A process"

}