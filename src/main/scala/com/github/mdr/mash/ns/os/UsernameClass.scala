package com.github.mdr.mash.ns.os

import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ Type, TypedArguments }
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.os.UserInteractions
import com.github.mdr.mash.runtime._
import org.apache.commons.lang3.SystemUtils

object UsernameClass extends MashClass("os.Username") {
  private val userInteractions = UserInteractions.default

  override lazy val methods =
    if (SystemUtils.IS_OS_MAC_OSX)
      Seq()
    else {
      val liftedFields = UserSummaryClass.fields.map(liftUserSummaryField)
      val liftedMethods = UserSummaryClass.methods.map(liftUserSummaryMethod)
      (InfoMethod +: liftedFields) ++ liftedMethods
    }

  private def liftUserSummaryField(field: Field) = new MashMethod(field.name) {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashValue = {
      val username = target.asInstanceOf[MashString].s
      val passwdEntry = userInteractions.passwdEntries.find(_.username == username).get
      val userSummary = UserSummaryClass.fromPasswdEntry(passwdEntry)
      userSummary.get(field.name).getOrElse(throw new EvaluatorException("No field found: " + field.name))
    }

    override def typeInferenceStrategy = UserSummaryClass.fieldsMap(field.name).fieldType

    override def summaryOpt = field.summaryOpt

    override def descriptionOpt = field.descriptionOpt

  }

  private def liftUserSummaryMethod(method: MashMethod) = new MashMethod(method.name) {

    val params = method.params

    override def call(target: MashValue, boundParams: BoundParams): MashValue = {
      val username = target.asInstanceOf[MashString].s
      val passwdEntry = userInteractions.passwdEntries.find(_.username == username).get
      val userSummary = UserSummaryClass.fromPasswdEntry(passwdEntry)
      method.call(userSummary, boundParams)
    }

    override def typeInferenceStrategy = method.typeInferenceStrategy

    override def getCompletionSpecs(argPos: Int, targetTypeOpt: Option[Type], arguments: TypedArguments) =
      method.getCompletionSpecs(argPos, targetTypeOpt, arguments)

    override def summaryOpt = method.summaryOpt

    override def descriptionOpt = method.descriptionOpt

  }

  object InfoMethod extends MashMethod("info") {

    val params = ParameterModel.Empty

    def call(target: MashValue, boundParams: BoundParams): MashObject = {
      val username = target.asInstanceOf[MashString].s
      val passwdEntry = userInteractions.passwdEntries.find(_.username == username).get
      UserSummaryClass.fromPasswdEntry(passwdEntry)
    }

    override def typeInferenceStrategy = UserSummaryClass

    override def summaryOpt = Some("Fetch information about the user with this username")

  }

  override def enumerationValues: Option[Seq[String]] = Some(userInteractions.passwdEntries.map(_.username).sorted)

  override def summaryOpt = Some("A username")

  override def parentOpt = Some(AnyClass)

}