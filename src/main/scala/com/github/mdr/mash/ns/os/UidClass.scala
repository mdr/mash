package com.github.mdr.mash.ns.os

import com.github.mdr.mash.classes.{ Field, MashClass }
import com.github.mdr.mash.evaluator.EvaluatorException
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ Type, TypedArguments }
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.os.UserInteractions
import com.github.mdr.mash.runtime.{ MashNumber, MashObject, MashValue }

object UidClass extends MashClass("os.Uid") {

  private val userInteractions = UserInteractions.default

  override lazy val methods = {
    val liftedFields = UserSummaryClass.fields.map(liftUserSummaryField)
    val liftedMethods = UserSummaryClass.methods.map(liftUserSummaryMethod)
    (InfoMethod +: liftedFields) ++ liftedMethods
  }

  private def liftUserSummaryField(field: Field) = new MashMethod(field.name) {

    val params = ParameterModel()

    def call(target: MashValue, boundParams: BoundParams): MashValue = {
      val uid = target.asInstanceOf[MashNumber].asInt.get
      val passwdEntry = userInteractions.passwdEntries.find(_.uid == uid).get
      val userSummary = UserSummaryClass.fromPasswdEntry(passwdEntry)
      userSummary.get(field.name).getOrElse(throw new EvaluatorException("No field found: " + field.name))
    }

    override def typeInferenceStrategy = UserSummaryClass.fieldsMap(field.name).fieldType

    override def summaryOpt = Some(s"Access '${field.name}' property of this user")

    override def descriptionOpt = field.descriptionOpt

  }

  private def liftUserSummaryMethod(method: MashMethod) = new MashMethod(method.name) {

    val params = method.params

    override def call(target: MashValue, boundParams: BoundParams): MashValue = {
      val uid = target.asInstanceOf[MashNumber].asInt.get
      val passwdEntry = userInteractions.passwdEntries.find(_.uid == uid).get
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

    val params = ParameterModel()

    def call(target: MashValue, boundParams: BoundParams): MashObject = {
      val uid = target.asInstanceOf[MashNumber].asInt.get
      val passwdEntry = userInteractions.passwdEntries.find(_.uid == uid).get
      UserSummaryClass.fromPasswdEntry(passwdEntry)
    }

    override def typeInferenceStrategy = UserSummaryClass

    override def summaryOpt = Some("Fetch information about the user with this uid")

  }

  override def summaryOpt = Some("Tag class for user ID (UID)")

  override def parentOpt = Some(AnyClass)

}