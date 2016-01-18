package com.github.mdr.mash.ns.os

import scala.collection.immutable.ListMap
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.os.ProcessInfo
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.os.linux.LinuxProcessInteractions
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel

object PidClass extends MashClass("os.Pid") {

  private val processInteractions = LinuxProcessInteractions

  override lazy val methods = {
    val liftedMethods = ProcessClass.methods.map(liftProcessMethod)
    val liftedFields = ProcessClass.fields.map(liftProcessField)
    (InfoMethod +: liftedMethods) ++ liftedFields
  }

  private case class Wrapper(target: Any) {

    val pid = target.asInstanceOf[MashNumber].asInt.getOrElse(throw new EvaluatorException("Invalid pid: " + target))

  }

  private def liftProcessMethod(method: MashMethod) = new MashMethod(method.name) {

    val params = method.params

    def apply(target: Any, arguments: Arguments): Any = {
      val pid = Wrapper(target).pid
      val processInfo = PidClass.getProcessInfo(pid)
      val processObject = ProcessClass.makeProcess(processInfo)
      method.apply(processObject, arguments)
    }

    override def typeInferenceStrategy = method.typeInferenceStrategy

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) =
      method.getCompletionSpecs(argPos, targetTypeOpt, arguments)

    override def summary = method.summary

    override def descriptionOpt = method.descriptionOpt

  }

  private def liftProcessField(field: Field) = new MashMethod(field.name) {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): Any = {
      params.validate(arguments)
      val pid = Wrapper(target).pid
      val processInfo = PidClass.getProcessInfo(pid)
      val processObject = ProcessClass.makeProcess(processInfo)
      processObject.fields(field.name)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(ProcessClass.fieldsMap(field.name).fieldType)

    override def summary = s"Access '$field' property of this process"

    override def descriptionOpt = field.descriptionOpt

  }

  object InfoMethod extends MashMethod("info") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashObject = {
      params.validate(arguments)
      val pid = Wrapper(target).pid
      val processInfo = getProcessInfo(pid)
      ProcessClass.makeProcess(processInfo)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(ProcessClass))

    override def summary = "Get information about the process with this pid"

  }

  private def getProcessInfo(pid: Int): ProcessInfo =
    processInteractions.getProcess(pid).getOrElse(throw new EvaluatorException("Cannot find process with pid " + pid))

  override def enumerationValues: Option[Seq[String]] =
    Some(processInteractions.getProcesses.map(_.pid).map(_.toString))

  override def summary = "Tag class for process ID (PID)"

}