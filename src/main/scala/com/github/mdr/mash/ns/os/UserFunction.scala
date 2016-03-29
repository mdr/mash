package com.github.mdr.mash.ns.os

import scala.collection.JavaConverters._
import com.github.mdr.mash.evaluator._
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.os.linux.LinuxUserInteractions

object UserFunction extends MashFunction("os.user") {

  private val userInteractions = LinuxUserInteractions

  val params = ParameterModel()

  def apply(arguments: Arguments): MashObject = {
    params.validate(arguments)
    val username = userInteractions.currentUsername
    val passwdEntry = userInteractions.passwdEntries.find(_.username == username).getOrElse(
      throw new EvaluatorException(s"Could not find full user information for user '$username'"))
    UserSummaryClass.fromPasswdEntry(passwdEntry)
  }
  
  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Instance(UserSummaryClass))

  override def summary = "The current user"

}