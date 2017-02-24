package com.github.mdr.mash.ns.os

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.os.ProcessInfo
import com.github.mdr.mash.os.linux.LinuxProcessInteractions
import com.github.mdr.mash.runtime._

import scala.collection.immutable.ListMap

object ProcessClass extends MashClass("os.Process") {

  private val processInteractions = LinuxProcessInteractions

  object Fields {
    val Pid = Field("pid", Some("Id of process"), NumberClass taggedWith PidClass)
    val ParentPid = Field("parentPid", Some("Id of parent process"), NumberClass taggedWith PidClass)
    val Name = Field("name", Some("Name"), StringClass)
    val Command = Field("command", Some("Command"), StringClass)
    val Owner = Field("owner", Some("Owner"), StringClass taggedWith UsernameClass)
    val ResidentSize = Field("residentSize", Some("Resident set size (RSS) in bytes"), NumberClass taggedWith BytesClass)
    val VirtualSize = Field("virtualSize", Some("Virtual memory size (VSZ) in bytes"), NumberClass taggedWith BytesClass)
  }

  import Fields._

  override val fields = Seq(Pid, ParentPid, Name, Command, Owner, ResidentSize, VirtualSize)

  override val methods = Seq(
    ChildrenMethod,
    KillMethod,
    ParentMethod)

  override val staticMethods = Seq(NewStaticMethod(this))

  case class Wrapper(value: MashValue) extends AbstractObjectWrapper(value) {

    def pid: Int = getNumberField(Pid).toInt

    def parentPidOpt: Option[Int] =
      getOptionalField(ParentPid).map(_.asInstanceOf[MashNumber]).flatMap(_.asInt)

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

    def apply(target: MashValue, boundParams: BoundParams): MashUnit = {
      val pid = Wrapper(target).pid
      val signal = KillFunction.getSignal(boundParams, Params.Signal)
      processInteractions.kill(pid, signal)
      MashUnit
    }

    override def typeInferenceStrategy = UnitClass

    override def summaryOpt = Some("Kill this process")

    override def descriptionOpt = Some("""Examples:
  process.kill "HUP" # Send the HUP signal the the process""")

  }

  object ParentMethod extends MashMethod("parent") {

    val params = ParameterModel()

    def apply(target: MashValue, boundParams: BoundParams): MashValue = {
      val parentPidOpt = Wrapper(target).parentPidOpt
      val parentProcessOpt =
        for {
          parentPid ← parentPidOpt
          processInfo ← processInteractions.getProcess(parentPid)
        } yield makeProcess(processInfo)
      parentProcessOpt.getOrElse(MashNull)
    }

    override def typeInferenceStrategy = ProcessClass

    override def summaryOpt = Some("The parent process")

  }

  object ChildrenMethod extends MashMethod("children") {

    val params = ParameterModel()

    def apply(target: MashValue, boundParams: BoundParams): MashList = {
      val pid = Wrapper(target).pid
      val children = processInteractions.getProcesses.filter(_.parentPidOpt contains pid)
      MashList(children.map(makeProcess))
    }

    override def typeInferenceStrategy = Type.Seq(ProcessClass)

    override def summaryOpt = Some("Children of this process")

  }

  override def summaryOpt = Some("A process")

}