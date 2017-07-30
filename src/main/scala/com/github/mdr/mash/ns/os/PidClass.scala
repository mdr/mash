package com.github.mdr.mash.ns.os

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass }
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ Type, TypedArguments }
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.os.ProcessInfo
import com.github.mdr.mash.os.linux.LinuxProcessInteractions
import com.github.mdr.mash.runtime.{ MashNumber, MashObject, MashValue }

object PidClass extends MashClass("os.Pid") {

  private val processInteractions = LinuxProcessInteractions

  override lazy val methods = {
    val liftedMethods = ProcessClass.methods.map(liftProcessMethod)
    val liftedFields = ProcessClass.fields.map(liftProcessField)
    (InfoMethod +: liftedMethods) ++ liftedFields
  }

  private case class Wrapper(value: MashValue) extends AbstractObjectWrapper(value) {

    val pid = target.asInstanceOf[MashNumber].asInt.getOrElse(throw new EvaluatorException("Invalid pid: " + target))

  }

  private def liftProcessMethod(method: MashMethod) = new MashMethod(method.name) {

    val params = method.params

    override def call(target: MashValue, boundParams: BoundParams): MashValue = {
      val pid = Wrapper(target).pid
      val processInfo = PidClass.getProcessInfo(pid)
      val processObject = ProcessClass.makeProcess(processInfo)
      method.call(processObject, boundParams)
    }

    override def typeInferenceStrategy = method.typeInferenceStrategy

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) =
      method.getCompletionSpecs(argPos, targetTypeOpt, arguments)

    override def summaryOpt = method.summaryOpt

    override def descriptionOpt = method.descriptionOpt

  }

  private def liftProcessField(field: Field) = new MashMethod(field.name) {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashValue = {
      val pid = Wrapper(target).pid
      val processInfo = PidClass.getProcessInfo(pid)
      val processObject = ProcessClass.makeProcess(processInfo)
      processObject.get(field.name).getOrElse(throw new EvaluatorException("No field found: " + field.name))
    }

    override def typeInferenceStrategy = ProcessClass.fieldsMap(field.name).fieldType

    override def summaryOpt = field.summaryOpt

    override def descriptionOpt = field.descriptionOpt

  }

  object InfoMethod extends MashMethod("info") {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashObject = {
      val pid = Wrapper(target).pid
      val processInfo = getProcessInfo(pid)
      ProcessClass.makeProcess(processInfo)
    }

    override def typeInferenceStrategy = ProcessClass

    override def summaryOpt = Some("Get information about the process with this pid")

  }

  private def getProcessInfo(pid: Int): ProcessInfo =
    processInteractions.getProcess(pid).getOrElse(throw new EvaluatorException("Cannot find process with pid " + pid))

  override def enumerationValues: Option[Seq[String]] =
    Some(processInteractions.getProcesses.map(_.pid).map(_.toString))

  override def summaryOpt = Some("Tag class for process ID (PID)")

  override def parentOpt = Some(AnyClass)

}